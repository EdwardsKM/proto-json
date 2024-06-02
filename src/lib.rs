extern crate proc_macro;
extern crate syn;
use quote::quote;
use syn::{
    fold::{fold_type, Fold},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    visit::Visit,
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Field, Fields, LitStr, Path, PathSegment,
    Type, TypePath,
};

/// Helps to glue together json and protobufs when placed on a valid prost::Message, prost::Enumeration, or prost::Oneof
/// For structs, it walks the fields checking whether they are enums then adds serialize_with and deserialize_with attributes to relevant fields.
/// Structs need to have the prost::Message attribute while Enums require the prost::Enumeration attribute
/// For enums, it checks that the provided string is a valid variant then deserializes it as i32 to match the protobuf definitions
/// # Example
/// ```
/// # #[macros::proto_json]
/// pub struct Address {
///     country : String,
///     city : String,
///     state : Option<String>,
///     street : String,
///     line1   : String,
///     line2   : Option<String>
/// # }
/// ```
/// Example with enums
/// ```
/// # #[macros::proto_json]
/// pub enum Currency {
///         USD = 0;
///         GPB = 1;
///         JPY = 2;
/// # }
/// ```
#[proc_macro_attribute]
pub fn proto_json(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> ::proc_macro::TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;

    let mut is_prost_message = false;
    let mut is_prost_enumeration = false;
    let mut is_prost_one_of = false;

    // If item does not implement one of the following attributes, then return error as it is not a valid protobuf object.
    for attrib in ast.attrs.iter() {
        if attrib.path().is_ident("derive") {
            attrib
                .parse_nested_meta(|meta| {
                    match meta.path.leading_colon {
                        Some(_) => match meta
                            .path
                            .segments
                            .last()
                            .unwrap()
                            .ident
                            .to_string()
                            .as_str()
                        {
                            "Message" => is_prost_message = true,
                            "Enumeration" => is_prost_enumeration = true,
                            "Oneof" => is_prost_one_of = true,
                            _ => (),
                        },
                        None => (),
                    }
                    Ok(())
                })
                .unwrap();
        }
    }

    let generated: proc_macro2::TokenStream = match ast.data {
        Data::Enum(ref mut de) => {
            match is_prost_enumeration {
                // Implement the str_to_i32 and i32_to_str methods
                true => {
                    // Iterate over enum variants
                    let variants = de.variants.iter().map(|v| &v.ident);

                    // Convert variant name to snake_case
                    let variant_str_as_i32 = variants.clone().map(|variant| {
                        let variant_str = &::convert_case::Casing::to_case(
                            &variant.to_string(),
                            ::convert_case::Case::Snake,
                        );
                        // usd => Ok(Currency::Usd as i32)
                        quote! (#variant_str => Ok(#ident::#variant as i32))
                    });

                    // Creates a list of the enum's fields. Used to give useful error messages when deserializing.
                    let expected_fields = variants
                        .clone()
                        .map(|variant| {
                            let variant_str = ::convert_case::Casing::to_case(
                                &variant.to_string(),
                                ::convert_case::Case::Snake,
                            );
                            variant_str
                        })
                        .into_iter()
                        .map(|x| x)
                        .collect::<Vec<String>>()
                        .join(",");

                    let serde_funcs = quote! {
                        /// Methods for converting from Protofbuf to and from Json enums
                        impl #ident {
                            /// Deserialize enum from string to protobuf i32
                            pub fn  str_to_i32<'de, D>(deserializer: D) -> core::result::Result<i32, D::Error>
                            where
                                D: serde::de::Deserializer<'de>,
                            {
                                let s: &str = serde::de::Deserialize::deserialize(deserializer)?;

                                match s.to_lowercase().as_str() {
                                    #(#variant_str_as_i32,)*
                                    _ => core::result::Result::Err(serde::de::Error::unknown_variant(s, &[#expected_fields])),
                                }
                            }
                            /// Deserialize optional enum from string to optional protobuf i32
                            pub fn str_to_i32_opt<'de, D>(deserializer: D) -> core::result::Result<Option<i32>, D::Error>
                            where
                                D: serde::de::Deserializer<'de>,
                            {
                                let s: Option<&str> = serde::de::Deserialize::deserialize(deserializer)?;

                                if let Some(s) = s {
                                    return Ok(Some(
                                        s.to_lowercase()
                                            .as_str()
                                            .parse::<Self>()
                                            .map_err(|_| serde::de::Error::unknown_variant(s, &[#expected_fields]))?
                                            as i32));
                                }

                                Ok(None)
                            }
                            /// Serialize enum from protobuf i32 to json string
                            pub fn i32_to_str<S>(data: &i32, serializer: S) -> core::result::Result<S::Ok, S::Error>
                            where
                                S: serde::Serializer,
                            {
                                serializer.serialize_str(&Self::try_from(data.to_owned()).unwrap().to_string())
                            }
                            /// Serialize enum from optional protobuf i32 to optional json string
                            pub fn i32_to_str_opt<S>(
                                data: &Option<i32>,
                                serializer: S,
                            ) -> core::result::Result<S::Ok, S::Error>
                            where
                                S: serde::Serializer,
                            {
                                if let Some(ref d) = *data {
                                    return serializer.serialize_str(&Self::try_from(d.to_owned()).unwrap().to_string());
                                }
                                serializer.serialize_none()
                            }
                            // Deserialize from a vec string to a vec i32
                            pub fn vec_str_to_vec_i32<'de, D>(deserializer: D) -> core::result::Result<Vec<i32>, D::Error>
                            where
                                D: serde::de::Deserializer<'de>,
                            {
                                let strings: Vec<&str> = serde::de::Deserialize::deserialize(deserializer)?;

                                let mut result = Vec::with_capacity(strings.len());

                                for s in strings {
                                    match s.parse::<Self>() {
                                        Ok(num) => result.push(num as i32),
                                        Err(_) => {
                                            return Err(serde::de::Error::invalid_value(
                                                serde::de::Unexpected::Str(s),
                                                &#expected_fields,
                                            ))
                                        }
                                    }
                                }

                                Ok(result)
                            }
                            // Serializes a vec of enum i32s to a vec of strings
                            pub fn vec_i32_to_vec_str<S>(
                                data: &Vec<i32>,
                                serializer: S,
                            ) -> core::result::Result<S::Ok, S::Error>
                            where
                                S: serde::Serializer,
                            {
                                let mut seq = serializer.serialize_seq(Some(data.len()))?;

                                for &i in data {
                                    serde::ser::SerializeSeq::serialize_element(
                                        &mut seq,
                                        &Self::try_from(i.to_owned()).unwrap().to_string(),
                                    )?;
                                }

                                serde::ser::SerializeSeq::end(seq)
                            }
                        }

                    };

                    quote! {
                        #ast
                        #serde_funcs
                    }
                    .into()
                }
                // Check if the given item has the prost::Oneof attribute which indicates an enum nested inside a module
                false => match is_prost_one_of {
                    true => {
                        let attribute: Attribute = parse_quote! {
                            #[derive(serde::Serialize, serde::Deserialize)]
                        };

                        let attribute2: Attribute = parse_quote!(
                            #[serde(rename_all = "snake_case")]
                        );

                        ast.attrs.push(attribute);
                        ast.attrs.push(attribute2);

                        // Generate a new TokenTree of data type DataEnum
                        let new = Data::Enum(DataEnum {
                            enum_token: de.enum_token,
                            brace_token: de.brace_token,
                            variants: de.variants.clone(),
                        });

                        // Copy over the attributes from the original enum for max compatibility
                        let new_enum = DeriveInput {
                            attrs: ast.attrs,
                            vis: ast.vis,
                            ident: ident.clone(),
                            generics: ast.generics,
                            data: new,
                        };

                        quote! {#new_enum}.into()
                    }
                    false => {
                        return ::syn::Error::new_spanned(
                            &ident,
                            "Could not parse the item as a valid protobuf Enum",
                        )
                        .to_compile_error()
                        .into();
                    }
                },
            }
        }
        // A struct that implements prost::Message
        ::syn::Data::Struct(ref mut ds) => match &ds.fields {
            Fields::Named(fields) => {
                match is_prost_message {
                    true => {
                        let mut new_fields = fields.to_owned();

                        new_fields.named.iter_mut().for_each(|f| match is_option(f) {
                            true => {
                                match check_struct_field_for_prost_enumeration_attribute(&f.attrs) {
                                    Some(a) => {
                                        match check_is_vec(&f.ty) {
                                            // check whether field is vec 
                                            true => {
                                                let serializer = format!("{a}::vec_i32_to_vec_str");
                                                let deserializer = format!("{a}::vec_str_to_vec_i32");

                                                // Create a new serialize_with, deserialize_with attribute
                                                let new_attr: Attribute = parse_quote! {
                                                    #[serde( default, deserialize_with = #deserializer, serialize_with = #serializer)]
                                                };

                                                f.attrs.push(new_attr);
                                            },
                                            false => {
                                                let serializer = format!("{a}::i32_to_str_opt");
                                                let deserializer = format!("{a}::str_to_i32_opt");

                                                // Create a new serialize_with, deserialize_with attribute
                                                let new_attr: Attribute = parse_quote! {
                                                    #[serde( default, deserialize_with = #deserializer, serialize_with = #serializer, skip_serializing_if = "Option::is_none" )]
                                                };

                                                f.attrs.push(new_attr);
                                            }
                                        }
                                    },
                                    None => ()
                                }
                            }
                            false => {

                                match check_struct_field_for_prost_enumeration_attribute(&f.attrs) {
                                    Some(a) => {
                                        match check_is_vec(&f.ty) {
                                            true => {
                                                let serializer = format!("{a}::vec_i32_to_vec_str");
                                                let deserializer = format!("{a}::vec_str_to_vec_i32");
                                                // Create a new serialize_with, deserialize_with attribute
                                                let new_attr: Attribute = parse_quote! {
                                                    #[serde(default, deserialize_with = #deserializer, serialize_with = #serializer)]
                                                };
                                                f.attrs.push(new_attr);
                                            },
                                            false => {
                                                let serializer = format!("{a}::i32_to_str");
                                                let deserializer = format!("{a}::str_to_i32");
                                                // Create a new serialize_with, deserialize_with attribute
                                                let new_attr: Attribute = parse_quote! {
                                                    #[serde(default, deserialize_with = #deserializer, serialize_with = #serializer)]
                                                };
                                                f.attrs.push(new_attr);
                                            }
                                        }
                                    },
                                    None => ()
                                }
                            }
                        });

                        let new = Data::Struct(DataStruct {
                            struct_token: ds.struct_token,
                            fields: Fields::Named(new_fields),
                            semi_token: ds.semi_token,
                        });

                        let new_st = DeriveInput {
                            attrs: ast.attrs,
                            vis: ast.vis,
                            ident: ident.clone(),
                            generics: ast.generics,
                            data: new,
                        };

                        quote! {#new_st}.into()
                    }
                    false => {
                        return ::syn::Error::new_spanned(
                            &ident,
                            "ProtoJson only works with Protobuf Structs",
                        )
                        .to_compile_error()
                        .into();
                    }
                }
            }
            _ => {
                return ::syn::Error::new_spanned(
                    &ds.fields,
                    "ProtoJson only supports named field structs",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return ::syn::Error::new_spanned(
                &ident,
                "Only items with named fields can derive ProtoJson",
            )
            .to_compile_error()
            .into();
        }
    };

    ::proc_macro::TokenStream::from(generated)
}

/// Checks if a struct field is Optional
fn is_option(field: &Field) -> bool {
    let typ = &field.ty;

    let opt = match typ {
        Type::Path(typepath) if typepath.qself.is_none() => Some(typepath.path.clone()),
        _ => None,
    };

    if let Some(o) = opt {
        check_for_option(&o).is_some()
    } else {
        false
    }
}

/// Walks the path segments to check for Option
fn check_for_option(path: &Path) -> Option<&PathSegment> {
    let idents_of_path = path.segments.iter().fold(String::new(), |mut acc, v| {
        acc.push_str(&v.ident.to_string());
        acc.push(':');
        acc
    });
    vec!["Option:", "std:option:Option:", "core:option:Option:"]
        .into_iter()
        .find(|s| idents_of_path == *s)
        .and_then(|_| path.segments.last())
}


/// Checks whether the attribute has a prost-enumeration member and returns an optional ident of the name
fn check_struct_field_for_prost_enumeration_attribute(attrs: &[Attribute]) -> Option<String> {
    // If a #[prost(enumeration = "value")] exists, this is the optional name of the value
    let mut attrib: Option<String> = None;

    //
    for attr in attrs {
        // Looks for attributes in the form #[prost(enumeration = "Currency", tag = "2")]
        if attr.path().is_ident("prost") {
            // Check for the "enumeration" part in the enumeration = "Currency" and parse it as KV.
            attr.parse_nested_meta(|meta| {
                // #[prost(enumeration = "Ident")]
                if meta.path.is_ident("enumeration") {
                    // Get the Currency in enumeration = "Currency". Gives back a string literal (LitStr)
                    let value = meta.value().unwrap();
                    //
                    attrib = Some(value.parse::<LitStr>().unwrap().value());

                    return Ok(());
                // If there is no enumeration attribute, do nothing
                } else {
                    Ok(())
                }
            })
            .unwrap_or(());
        }
    }
    attrib
}

struct VecTypeVisitor {
    is_vec: bool,
}

impl<'ast> Visit<'ast> for VecTypeVisitor {
    fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
        if segment.ident == "Vec" {
            self.is_vec = true;
        }
    }
}

impl Fold for VecTypeVisitor {
    fn fold_type_path(&mut self, type_path: TypePath) -> TypePath {
        let mut new_segments = Punctuated::new();

        for segment in type_path.path.segments {
            new_segments.push(self.fold_path_segment(segment));
        }

        TypePath {
            qself: type_path.qself,
            path: syn::Path {
                leading_colon: type_path.path.leading_colon,
                segments: new_segments,
            },
        }
    }

    fn fold_path_segment(&mut self, segment: PathSegment) -> PathSegment {
        let new_segment = segment.clone();
        if segment.ident == "Vec" {
            self.is_vec = true;
        }
        new_segment
    }

    fn fold_type(&mut self, ty: Type) -> Type {
        match ty {
            Type::Path(type_path) => Type::Path(self.fold_type_path(type_path)),
            Type::Tuple(type_tuple) => {
                let new_elems = type_tuple
                    .elems
                    .into_iter()
                    .map(|ty| self.fold_type(ty))
                    .collect();
                Type::Tuple(syn::TypeTuple {
                    paren_token: type_tuple.paren_token,
                    elems: new_elems,
                })
            }
            // Add cases for other types as needed
            _ => ty,
        }
    }
}

fn check_is_vec(ty: &syn::Type) -> bool {
    let mut visitor = VecTypeVisitor { is_vec: false };
    fold_type(&mut visitor, ty.clone());
    visitor.is_vec
}

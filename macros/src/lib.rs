use attribute_derive::Attribute;
use proc_macro2::TokenStream;
use proc_macro_error::ResultExt;
use quote_use::quote_use as quote;
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed,
    Ident, Type,
};

#[derive(Attribute)]
#[attribute(ident = "parse")]
struct ParseFieldAttribute {
    inner: Option<Ident>,
    complete: bool,
    seperator: Option<Type>,
}

#[proc_macro_derive(Parse, attributes(parse))]
pub fn derive_parse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input as DeriveInput);

    let inner = match data {
        Data::Struct(DataStruct { fields, .. }) => match fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                let mut inners: Vec<Ident> = vec![];
                let fields: Vec<_> = named
                    .into_iter()
                    .map(|Field { ident, attrs, .. }| {
                        let ParseFieldAttribute {
                            inner,
                            complete,
                            seperator,
                        } = ParseFieldAttribute::from_attributes(attrs.iter()).unwrap();
                        if let Some(inner) = inner {
                            let ret = quote!(#ident: delimited!(#inner in $input));
                            inners.push(inner);
                            ret
                        } else {
                            let input = if inners.contains(ident.as_ref().unwrap()) {
                                quote!((&mut #ident))
                            } else {
                                quote!($input)
                            };
                            if let Some(seperator) = seperator {
                                if complete {
                                    quote!(#ident: #input.parse_seperated_complete::<_, #seperator>()?)
                                } else {
                                    quote!(#ident: #input.parse_seperated::<_, #seperator>()?)
                                }
                            } else {
                                quote!(#ident: #input.parse()?)
                            }
                        }
                    })
                    .collect();
                // paren: delimited!(args in tokens),
                quote! {
                    #(let mut #inners;)*
                    Ok(Self{
                        #(#fields),*
                    })
                }
            }
            Fields::Unnamed(_) => unimplemented!(),
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(data) => {
            let variants = data.variants.into_iter().map(|variant| variant.ident);
            quote! {
                any! {$input:
                    #(value => Self::#variants(value),)*
                };
                unexpected!(expected #ident in $input);
            }
        }
        Data::Union(_) => unimplemented!(),
    };
    quote! {
        // # use crate::syn::{Parse, ParseBuffer};
        impl crate::syn::Parse for #ident {
            fn parse($input: &mut crate::syn::ParseBuffer) -> crate::syn::Result<Self> {
                #inner
            }
        }
    }
    .into()
}

#[derive(Attribute)]
#[attribute(ident = "lisp")]
struct LispItemAttribute {
    #[attribute(positional)]
    lisp: Option<TokenStream>,
}

#[proc_macro_derive(ToLisp, attributes(lisp))]
pub fn derive_to_lisp(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        ident, data, attrs, ..
    } = parse_macro_input!(input as DeriveInput);
    let LispItemAttribute { lisp } = LispItemAttribute::from_attributes(&attrs).unwrap_or_abort();

    let inner = match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            let idents = named.into_iter().map(|f| f.ident);
            quote! {
                #[allow(unused)]
                let Self{#(#idents,)*} = self;
                lisp!(#lisp)
            }
        }
        Data::Struct(_) => unimplemented!("tuple structs not supported"),
        // Data::Struct(DataStruct { fields, .. }) => match fields {
        //     Fields::Named(FieldsNamed { named, .. }) => {
        //         let mut inners: Vec<Ident> = vec![];
        //         let fields: Vec<_> = named
        //             .into_iter()
        //             .map(|Field { ident, attrs, .. }| {
        //                 let ParseFieldAttribute {
        //                     inner,
        //                     complete,
        //                     seperator,
        //                 } = ParseFieldAttribute::from_attributes(attrs.iter()).unwrap();
        //                 if let Some(inner) = inner {
        //                     let ret = quote!(#ident: delimited!(#inner in $input));
        //                     inners.push(inner);
        //                     ret
        //                 } else {
        //                     let input = if inners.contains(ident.as_ref().unwrap()) {
        //                         quote!((&mut #ident))
        //                     } else {
        //                         quote!($input)
        //                     };
        //                     if let Some(seperator) = seperator {
        //                         if complete {
        //                             quote!(#ident: #input.parse_seperated_complete::<_, #seperator>()?)
        //                         } else {
        //                             quote!(#ident: #input.parse_seperated::<_, #seperator>()?)
        //                         }
        //                     } else {
        //                         quote!(#ident: #input.parse()?)
        //                     }
        //                 }
        //             })
        //             .collect();
        //         // paren: delimited!(args in tokens),
        //         quote! {
        //             #(let mut #inners;)*
        //             Ok(Self{
        //                 #(#fields),*
        //             })
        //         }
        //     }
        //     Fields::Unnamed(_) => unimplemented!(),
        //     Fields::Unit => unimplemented!(),
        // },
        Data::Enum(data) => {
            let variants = data.variants.into_iter().map(|variant| variant.ident);
            quote! {
                match self {
                    #(Self::#variants(v) => v.to_lisp(),)*
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    };
    quote! {
        // # use crate::syn::{Parse, ParseBuffer};
        impl crate::syn::lisp::ToLisp for #ident {
            fn to_lisp(&self) -> Lisp {
                #inner
            }
        }
    }
    .into()
}

#[derive(Attribute)]
#[attribute(ident = "span")]
struct SpanAttribute {
    ignored: bool,
}

#[proc_macro_derive(Spanned, attributes(span))]
pub fn derive_spanned(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input as DeriveInput);

    let inner = match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            let idents = named
                .into_iter()
                .filter(|field| {
                    !SpanAttribute::from_attributes(&field.attrs)
                        .unwrap()
                        .ignored
                })
                .map(|f| f.ident);
            quote! {
                #(self.#idents.span())+*
            }
        }
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(FieldsUnnamed { unnamed, .. }),
            ..
        }) => {
            // FIXME ignore
            let idents = 0..unnamed.len();
            quote! {
                #(self.#idents.span())+*
            }
        }
        Data::Struct(_) => unimplemented!("tuple structs not supported"),
        // Data::Struct(DataStruct { fields, .. }) => match fields {
        //     Fields::Named(FieldsNamed { named, .. }) => {
        //         let mut inners: Vec<Ident> = vec![];
        //         let fields: Vec<_> = named
        //             .into_iter()
        //             .map(|Field { ident, attrs, .. }| {
        //                 let ParseFieldAttribute {
        //                     inner,
        //                     complete,
        //                     seperator,
        //                 } = ParseFieldAttribute::from_attributes(attrs.iter()).unwrap();
        //                 if let Some(inner) = inner {
        //                     let ret = quote!(#ident: delimited!(#inner in $input));
        //                     inners.push(inner);
        //                     ret
        //                 } else {
        //                     let input = if inners.contains(ident.as_ref().unwrap()) {
        //                         quote!((&mut #ident))
        //                     } else {
        //                         quote!($input)
        //                     };
        //                     if let Some(seperator) = seperator {
        //                         if complete {
        //                             quote!(#ident: #input.parse_seperated_complete::<_, #seperator>()?)
        //                         } else {
        //                             quote!(#ident: #input.parse_seperated::<_, #seperator>()?)
        //                         }
        //                     } else {
        //                         quote!(#ident: #input.parse()?)
        //                     }
        //                 }
        //             })
        //             .collect();
        //         // paren: delimited!(args in tokens),
        //         quote! {
        //             #(let mut #inners;)*
        //             Ok(Self{
        //                 #(#fields),*
        //             })
        //         }
        //     }
        //     Fields::Unnamed(_) => unimplemented!(),
        //     Fields::Unit => unimplemented!(),
        // },
        Data::Enum(data) => {
            let variants = data.variants.into_iter().map(|variant| variant.ident);
            quote! {
                match self {
                    #(Self::#variants(v) => v.span(),)*
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    };
    quote! {
        // # use crate::syn::{Parse, ParseBuffer};
        impl crate::syn::Spanned for #ident {
            fn span(&self) -> crate::syn::Span {
                crate::syn::Span::EMPTY + #inner
            }
        }
    }
    .into()
}

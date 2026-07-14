use std::fs;
use std::path::Path;

use savvy_bindgen::{
    generate_c_header_file, generate_c_impl_file, generate_r_impl_file, merge_parsed_results,
    parse_file,
};

fn main() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let parsed = parse_file(&manifest.join("src/lib.rs"), &[]);
    let merged = merge_parsed_results(vec![parsed]).expect("Savvy definitions must merge");
    fs::write(manifest.join("api.h"), generate_c_header_file(&merged))
        .expect("write generated C header");
    fs::write(
        manifest.parent().unwrap().join("init.c"),
        generate_c_impl_file(&merged, "zigrSavvy"),
    )
    .expect("write generated C implementation");
    fs::write(
        manifest
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("R/000-wrappers.R"),
        generate_r_impl_file(&merged, "zigrSavvy"),
    )
    .expect("write generated R wrappers");
}

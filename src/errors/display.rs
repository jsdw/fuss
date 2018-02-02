use errors::errors::*;

pub fn display_error(e: ImportError) -> String {

    // match e {
    //         CannotImportNoPathSet => {

    //         },
    //         CannotOpenFile(path) => {

    //         },
    //         CannotReadFile(path) => {

    //         },
    //         ImportLoop(paths, path) => {

    //         },
    //         Import(boxed_err, path) => {

    //         }
    // }

    format!("{}", e)
}
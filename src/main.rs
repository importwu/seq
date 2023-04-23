use rtor::{
    Parser, 
    Input,
    Error,
    AsChar,
    ParseResult,
    primitive::{
        string_no_case
    }
};


fn main() {
    let r = keyword::from.parse("from a;");
    println!("{:?}", r);
    
}

mod keyword {
    use super::*;

    macro_rules! make_keyword {
        ($($name: ident), *) => {
            $(
                pub fn $name<I>(input: I) -> ParseResult<I, I> 
                where
                    I: Input,
                    I::Token: AsChar
                {
                    string_no_case(stringify!($name)).parse(input)
                }
            )* 
        };
    }

    make_keyword!(select, from);
}
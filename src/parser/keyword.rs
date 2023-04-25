use rtor::{
    Parser,
    Input,
    AsChar,
    ParseResult,
    primitive::string_no_case
};
    
macro_rules! define_keyword_parsers {
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

define_keyword_parsers!(
    SELECT, 
    FROM
);

#[test]
fn test() {
    let r = SELECT.parse("select * from t1");

    println!("{:?}", r)
}

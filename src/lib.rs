pub mod parser;
pub mod slow_parser;

#[cfg(test)]
mod tests {
    use super::slow_parser::*;
    #[test]
    fn test_parse_char() {
        assert_eq!(parse_char('a').run("abcd"), Ok('a'));
        assert_ne!(parse_char('a').run("bcd"), Ok('a'));
    }

    #[test]
    fn test_parse_str() {
        assert_eq!(parse_str("null").run("null"), Ok("null".into()));
        assert_ne!(parse_str("true").run("false"), Ok("true".into()));

        let mut p = parse_one_of([parse_str("true"), parse_str("truck")]);
        assert_eq!(p.run("truck"), Ok("truck".into()));
        assert_eq!(p.run("true"), Ok("true".into()));
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_bool().run("true"), Ok(true));
        assert_eq!(parse_bool().run("false"), Ok(false));
        assert_ne!(parse_bool().run("False"), Ok(false));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(parse_float().run("123"), Ok(123f64));
        assert_eq!(parse_float().run("-123.43"), Ok(-123.43));
        assert_eq!(parse_float().run("0.23"), Ok(0.23));
        assert_eq!(parse_float().run("23."), Ok(23.));
    }

    #[test]
    fn test_parse_list_of() {
        let parse_list_of_float = || parse_list_of(|| parse_float());

        assert_eq!(
            parse_list_of_float().run("[1.2, 2.3, 3.4]"),
            Ok(vec![1.2, 2.3, 3.4])
        );
        assert_eq!(parse_list_of_float().run("[1.2]"), Ok(vec![1.2]));
        assert_eq!(parse_list_of_float().run("[]"), Ok(vec![]));

        let parse_list_of_list_of_float = || {
            parse_list_of(|| parse_list_of_float())
                .run("[[1.2 , 2.2 ], [ 2.3], [3.4,1.0,2.0 ,3.0]]")
        };
        assert_eq!(
            parse_list_of_list_of_float(),
            Ok(vec![vec![1.2, 2.2], vec![2.3], vec![3.4, 1.0, 2.0, 3.0]])
        );
    }

    use super::parser::*;

    #[test]
    fn test_char_p() {
        let cp = char_p('a');
        assert_eq!(cp.run("abcd"), Ok('a'));
        assert_ne!(cp.run("bcd"), Ok('a'));
    }

    #[test]
    fn test_str_p() {
        assert_eq!(str_p("null").run("null"), Ok("null".to_string()));
        assert_ne!(str_p("true").run("false"), Ok("true".to_string()));

        let p = one_of_p([str_p("true"), str_p("truck")]);
        assert_eq!(p.run("truck"), Ok("truck".into()));
        assert_eq!(p.run("true"), Ok("true".into()));
    }

    #[test]
    fn test_bool_p() {
        let p = bool_p();
        assert_eq!(p.run("true"), Ok(true));
        assert_eq!(p.run("false"), Ok(false));
        assert_ne!(p.run("False"), Ok(false));
    }

    #[test]
    fn test_float_p() {
        assert_eq!(float_p().run("123"), Ok(123f64));
        assert_eq!(float_p().run("-123.43"), Ok(-123.43));
        assert_eq!(float_p().run("0.23"), Ok(0.23));
        assert_eq!(float_p().run("23."), Ok(23.));
    }

    #[test]
    fn test_list_of_p() {
        let parse_list_of_float = list_square_p(float_p());

        assert_eq!(
            parse_list_of_float.run("[1.2, 2.3, 3.4]"),
            Ok(vec![1.2, 2.3, 3.4])
        );
        assert_eq!(parse_list_of_float.run("[1.2]"), Ok(vec![1.2]));
        assert_eq!(parse_list_of_float.run("[]"), Ok(vec![]));

        let parse_list_of_list_of_float = list_square_p(parse_list_of_float);
        assert_eq!(
            parse_list_of_list_of_float.run("[[1.2 , 2.2 ], [ 2.3], [3.4,1.0,2.0 ,3.0]]"),
            Ok(vec![vec![1.2, 2.2], vec![2.3], vec![3.4, 1.0, 2.0, 3.0]])
        );
    }

    #[test]
    fn test_quoted_str_p() {
        assert_eq!(dq_str_p().run("\"abcd\""), Ok(format!("abcd")));
        assert_eq!(dq_str_p().run("\"abcd\"123"), Ok(format!("abcd")));
        assert_eq!(
            dq_str_p().run("\"ab\n 'c' \td\""),
            Ok(format!("ab\n 'c' \td"))
        );

        assert_eq!(sq_str_p().run("\'abcd\'"), Ok(format!("abcd")));
        assert_eq!(sq_str_p().run("\'abcd\'123"), Ok(format!("abcd")));
        assert_eq!(
            sq_str_p().run("\'ab\n \"c\" \td\'"),
            Ok(format!("ab\n \"c\" \td"))
        );
    }
}

mod lexer;
mod token;
fn main() {
    let input = "->";
    let (_, res) = lexer::token(input).unwrap();
    println!("{:?}", res);
}


enum Instr {

}

type VarEnv = HashMap<String, Value>;

enum Value {
    Unit,
    Bool(bool),
    Num(f32),
    Str(String),
    List(Vec<Value>),
    Fun(VarEnv, Vec<String>, Vec<Instr>),
}

type CalcStack = LinkedList<Value>;
type AppStack = LinkedList<Apps>;
type VarStack = LinkedList<

/*
Hindley-Milner type inference over (Lisp-ish) S-expressions (01/2017)

https://repl.it/FTwz/10

Copyright (c) 2017 Cyril Jandia

http://www.cjandia.com/

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
``Software''), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESSED
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL CYRIL JANDIA BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Cyril Jandia shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from Cyril Jandia.

Inquiries : ysharp {dot} design {at} gmail {dot} com
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Language.SExpressions;
using System.Language.TypeInference;

namespace Test
{   
    public class CtorInfo
    {
        public readonly string Cons;
        public readonly int Arity;
        public CtorInfo(string cons) : this(cons, 0) { }
        public CtorInfo(string cons, int arity) { Cons = cons; Arity = arity; }
        public IType GetType(IDictionary<string, IType> env) => env.Values.FirstOrDefault(type => (type.Meta != null) && (((CtorInfo)type.Meta).Cons == Cons) && (((CtorInfo)type.Meta).Arity == Arity));
    }

    class Program
    {
        static void Main(string[] args)
        {
            var syntax =
                new Syntax().
                Include
                (
                    // Required
                    Syntax.Lexical("'", Syntax.Quoting),

                    // Not-quite-Lisp-indeed; just stolen from our host, C#, as-is
                    Syntax.Lexical("\\/\\/.*", Syntax.Commenting),
                    Syntax.Lexical("false", (token, match) => false),
                    Syntax.Lexical("true", (token, match) => true),
                    Syntax.Lexical("null", (token, match) => null),

                    // Integers (unsigned)
                    Syntax.Lexical("[0-9]+", (token, match) => int.Parse(match)),

                    // String literals
                    Syntax.Lexical("\\\"(\\\\\\n|\\\\t|\\\\n|\\\\r|\\\\\\\"|[^\\\"])*\\\"", (token, match) => match.Substring(1, match.Length - 2)),

                    // For identifiers...
                    Syntax.Lexical("[\\$_A-Za-z][\\$_0-9A-Za-z\\-\\.]*", Syntax.NewSymbol),

                    // ... and such
                    Syntax.Lexical("[\\!\\&\\|\\<\\=\\>\\+\\-\\*\\/\\%\\,\\:@]+", Syntax.NewSymbol),
                    Syntax.Lexical("\\&\\&", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\|\\|", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\<\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\>\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\!\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\=\\=", Syntax.NewSymbol, true)
                );

            var system = TypeSystem.Default;
            var env = new Dictionary<string, IType>();

            // Classic
            var @void = system.NewType(typeof(void).FullName);
            var @bool = system.NewType(typeof(bool).FullName);
            var @int = system.NewType(typeof(int).FullName);
            var @string = system.NewType(typeof(string).FullName);

            // Generic pair type of some `first' and `second' types : Pair<first, second>
            // syntax for pairs:
            // <pair-sexpr> ::= '(' <sexpr> ',' <sexpr> ')'
            var PairType = system.NewType("Pair", new[] { system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 2));

            // Generic list type of some `item' type : List<item>;
            // syntax for lists:
            // <list-sexpr> ::= '(' <sexpr> ':' <list-sexpr> ')' | '(' ')'
            // (thus, "( )" is the empty list)
            var ListType = system.NewType("List", new[] { system.NewGeneric() }, new CtorInfo(":", 2));

            // Generic tuple types of some `item1', `item2', ... types : Tuple<item1, item2, ...>
            // syntax for tuples:
            // <tuple-sexpr> ::= '(' '|' <sexpr> ... ')' | '(' '|' ')'
            // (thus, "( | )" is the empty tuple)
            env["Tuple`0"] = system.NewType("Tuple`0", new CtorInfo("|", 0));
            env["Tuple`1"] = system.NewType("Tuple`1", new[] { system.NewGeneric() }, new CtorInfo("|", 1));
            env["Tuple`2"] = system.NewType("Tuple`2", new[] { system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 2));
            env["Tuple`3"] = system.NewType("Tuple`3", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 3));
            env["Tuple`4"] = system.NewType("Tuple`4", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 4));
            env["Tuple`5"] = system.NewType("Tuple`5", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 5));
            env["Tuple`6"] = system.NewType("Tuple`6", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 6));
            env["Tuple`7"] = system.NewType("Tuple`7", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 7));
            env["Tuple`8"] = system.NewType("Tuple`8", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 8));
            env["Tuple`9"] = system.NewType("Tuple`9", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo("|", 9));

            // Populate the top level typing environment
            env[@void.Id] = @void;
            env[@bool.Id] = @bool;
            env[@int.Id] = @int;
            env[@string.Id] = @string;
            env[PairType.Id] = PairType;
            env[ListType.Id] = ListType;

            // Bake some operator function types (to have something to infer about in familiar expression)
            var unary = system.NewGeneric();
            var binary0 = system.NewGeneric();
            var binary1 = system.NewGeneric();
            var binary2 = system.NewGeneric();
            var binary3 = system.NewGeneric();
            var binary4 = system.NewGeneric();
            var binary5 = system.NewGeneric();
            var binary6 = system.NewGeneric();
            var binary7 = system.NewGeneric();
            var binary8 = system.NewGeneric();
            var binary9 = system.NewGeneric();
            var binary10 = system.NewGeneric();
            var binary11 = system.NewGeneric();

            // boolean operators ( bool -> bool ) and ( bool -> bool -> bool )
            // (e.g., "( ! ( empty ( 0 : ( ) ) ) )")
            system.Infer(env, Node.Define(Node.Var("!"), Node.Abstract(new[] { Node.Var("expr", @bool) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("&&"), Node.Abstract(new[] { Node.Var("left", @bool), Node.Var("right", @bool) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("||"), Node.Abstract(new[] { Node.Var("left", @bool), Node.Var("right", @bool) }, @bool, Node.Const(@bool))));

            // Polymorphic operator types
            var asType = system.NewGeneric();
            var formal = system.NewGeneric();
            system.Infer(env, Node.Define(Node.Var("+"), Node.Abstract(new[] { Node.Var("left", binary1), Node.Var("right", binary1) }, binary1, Node.Var("left"))));
            system.Infer(env, Node.Define(Node.Var("-"), Node.Abstract(new[] { Node.Var("left", binary2), Node.Var("right", binary2) }, binary2, Node.Var("left"))));
            system.Infer(env, Node.Define(Node.Var("*"), Node.Abstract(new[] { Node.Var("left", binary3), Node.Var("right", binary3) }, binary3, Node.Var("left"))));
            system.Infer(env, Node.Define(Node.Var("/"), Node.Abstract(new[] { Node.Var("left", binary4), Node.Var("right", binary4) }, binary4, Node.Var("left"))));
            system.Infer(env, Node.Define(Node.Var("%"), Node.Abstract(new[] { Node.Var("left", binary5), Node.Var("right", binary5) }, binary5, Node.Var("left"))));
            system.Infer(env, Node.Define(Node.Var(">"), Node.Abstract(new[] { Node.Var("left", binary6), Node.Var("right", binary6) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var(">="), Node.Abstract(new[] { Node.Var("left", binary7), Node.Var("right", binary7) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("<"), Node.Abstract(new[] { Node.Var("left", binary8), Node.Var("right", binary8) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("<="), Node.Abstract(new[] { Node.Var("left", binary9), Node.Var("right", binary9) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("=="), Node.Abstract(new[] { Node.Var("left", binary10), Node.Var("right", binary10) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("!="), Node.Abstract(new[] { Node.Var("left", binary11), Node.Var("right", binary11) }, @bool, Node.Const(@bool))));

            // A ternary if-then-else will come in handy too
            var ifThenElse = system.NewGeneric();
            system.Infer(env, Node.Define(Node.Var("if"), Node.Abstract(new[] { Node.Var("condition", @bool), Node.Var("then", ifThenElse), Node.Var("else", ifThenElse) }, ifThenElse, Node.Var("then"))));

            system.Infer(env, Node.Define(Node.Var("count"), Node.Abstract(new[] { Node.Var("list", ListType) }, @int, Node.Const(@int))));
            system.Infer(env, Node.Define(Node.Var("empty"), Node.Abstract(new[] { Node.Var("list", ListType) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("head"), Node.Abstract(new[] { Node.Var("list", ListType) }, ListType[0], Node.Const(ListType[0]))));
            system.Infer(env, Node.Define(Node.Var("tail"), Node.Abstract(new[] { Node.Var("list", ListType) }, ListType, Node.Const(ListType))));
            system.Infer(env, Node.Define(Node.Var("first"), Node.Abstract(new[] { Node.Var("pair", PairType) }, PairType[0], Node.Const(PairType[0]))));
            system.Infer(env, Node.Define(Node.Var("second"), Node.Abstract(new[] { Node.Var("pair", PairType) }, PairType[1], Node.Const(PairType[1]))));
            system.Infer(env, Node.Define(Node.Var("length"), Node.Abstract(new[] { Node.Var("string", @string) }, @int, Node.Const(@int))));
            system.Infer(env, Node.Define(Node.Var("escape"), Node.Abstract(new[] { Node.Var("string", @string) }, @string, Node.Const(@string))));
            system.Infer(env, Node.Define(Node.Var("toString"), Node.Abstract(new[] { Node.Var("value", unary) }, @string, Node.Const(@string))));
            system.Infer(env, Node.Define(Node.Var("parseInt"), Node.Abstract(new[] { Node.Var("string", @string) }, @int, Node.Const(@int))));

            // DRY helpers
            var isFunction = null as Func<IType, bool>;
            isFunction = type => type != null ? (type.Constructor != null ? type.Constructor.Id == TypeSystem.Function.Id : isFunction(type.Self)) : false;
            Func<string, int, IType> typeOf = (id, arity) => id != null ? new CtorInfo(id, arity).GetType(env) : null;
            Func<string, int, bool> isCtor = (id, arity) => typeOf(id, arity) != null;
            Func<object, bool> isArray = value => value is object[];
            Func<object, object[]> array = value => (object[])value;
            Func<object, bool> isNil = value => isArray(value) && (array(value).Length == 0);
            Func<object[], bool> isSingleton = value => (value.Length == 3) && isNil(value[2]);
            var NIL = new object[0];

            // A sort of poor man's visitor (over the S-expr) : just a bunch of lambdas
            Func<object, Node> visitSExpr = null;
            Func<object, Let> visitLet = null;
            Func<object, Define> visitDefine = null;
            Func<object, Apply> visitApply = null;
            Func<object, int, Apply> visitCtor = null;
            Func<object, Abstract> visitAbstract = null;
            Func<object, Var> visitVar = null;
            Func<object, Const> visitConst = null;

            visitSExpr =
                sexpr =>
                {
                    var arity = (isArray(sexpr) ? array(sexpr).Length : 0) - 1;
                    var node =
                        isArray(sexpr) ?
                        (
                            // ( let / | / sexpr ... )-
                            array(sexpr).Length > 1 ?
                            (
                                (Syntax.Identifier(array(sexpr)[0]) == null) || (Syntax.Identifier(array(sexpr)[0]) != "let") ?
                                (
                                    // ( <ctor> / sexpr ... )
                                    Syntax.Identifier(array(sexpr)[1]) != "=>" ?
                                    (
                                        // ( sexpr ... )
                                        !isCtor(Syntax.Identifier(array(sexpr)[0]), arity) ?
                                        (
                                            !isCtor(Syntax.Identifier(array(sexpr)[1]), arity) ?
                                            // ( sexpr ... )
                                            visitApply(sexpr)
                                            :
                                            // ( sexpr <ctor> ... )
                                            visitCtor(sexpr, 1)
                                        )
                                        :
                                        // ( <ctor> ... )
                                        visitCtor(sexpr, 0)
                                    )
                                    :
                                    // ( ... => ... )
                                    (Node)visitAbstract(sexpr)
                                )
                                :
                                // ( let ... )
                                visitLet(sexpr)
                            )
                            :
                            (
                                // ( id / sexpr )
                                !isNil(sexpr) ?
                                (
                                    !isCtor(Syntax.Identifier(array(sexpr)[0]), 0) ?
                                    // ( sexpr )
                                    visitApply(sexpr)
                                    :
                                    // ( <ctor> )
                                    (Node)visitCtor(sexpr, 0)
                                )
                                :
                                // ( )
                                visitConst(ListType)
                            )
                        )
                        :
                        // id / const
                        Syntax.Identifier(sexpr) != null ? (Node)visitVar(sexpr) : visitConst(sexpr);
                    return node;
                };
            visitLet =
                sexpr =>
                {
                    var let = Node.Let(array(array(sexpr)[1]).Select(item => visitDefine(item)).ToArray(), visitSExpr(array(sexpr)[2]));
                    return let;
                };
            visitDefine =
                sexpr =>
                {
                    var define = Node.Define(visitVar(array(sexpr)[0]), visitSExpr(array(sexpr)[1]));
                    return define;
                };
            visitApply =
                sexpr =>
                {
                    Func<IType[], IType> newFunction = signature => system.NewType(TypeSystem.Function, signature);
                    Apply apply;
                    string id;
                    if (((id = Syntax.Identifier(array(sexpr)[0])) != null) && (!env.ContainsKey(id) || isFunction(env[id])))
                    {
                        var fn = Node.Var(id, !env.ContainsKey(id) ? env[id] = newFunction(array(sexpr).Skip(1).Select(arg => system.NewGeneric()).Concat(new[] { system.NewGeneric() }).ToArray()) : env[id]);
                        apply = Node.Apply(fn, array(sexpr).Skip(1).Select(arg => visitSExpr(arg)).ToArray());
                    }
                    else
                    {
                        apply =
                            Node.Apply
                            (
                                visitSExpr(array(sexpr)[0]),
                                array(sexpr).Skip(1).Select(arg => visitSExpr(arg)).ToArray(),
                                id != null ? env[id] : null
                            );
                    }
                    return apply;
                };
            visitCtor =
                (sexpr, at) =>
                {
                    var arity = array(sexpr).Length - 1;
                    var type = typeOf(Syntax.Identifier(array(sexpr)[at]), arity);
                    var ctor = Node.Apply(Node.Var(type.Id), new[] { array(sexpr)[1 - at] }.Concat(array(sexpr).Skip(2).Take(type.Args.Length - 1)).Select(arg => visitSExpr(arg)).ToArray(), type);
                    return ctor;
                };
            visitAbstract =
                sexpr =>
                {
                    var lambda =
                        Node.Abstract
                        (
                            array(array(sexpr)[0]).Select(arg => visitVar(arg)).ToArray(),
                            visitSExpr(array(sexpr)[2])
                        );
                    return lambda;
                };
            visitVar =
                sexpr =>
                {
                    var var = Node.Var(Syntax.Identifier(sexpr));
                    return var;
                };
            visitConst =
                sexpr =>
                {
                    var @const = Node.Const(sexpr);
                    return @const;
                };

            Action<Node> analyze =
                root =>
                {
                    Func<Node, IType> tryInfer =
                        node =>
                        {
                            try
                            {
                                return system.Infer(env, node);
                            }
                            catch (Exception ex)
                            {
                                return system.NewType(string.Format("Type error: {0}", ex.Message));
                            }
                        };
                    var sw = new System.Diagnostics.Stopwatch();
                    sw.Start();
                    foreach (var node in root.Args) // Pass 1 (handle letrec's, leaving some generic types open)
                    {
                        tryInfer(node);
                    }
                    foreach (var node in root.Args) // Pass 2 (close as many generic types as possible)
                    {
                        Console.WriteLine();
                        Console.WriteLine(node.Spec is Node ? string.Format("{0}:\r\n\t{1}", (Node)node.Spec, tryInfer(node)) : tryInfer(node).ToString());
                    }
                    sw.Stop();
                    Console.WriteLine();
                    Console.WriteLine(string.Format("{0}:\r\n\t{1}", root.Body, tryInfer(root)));
                    Console.WriteLine();
                    Console.WriteLine("... Done (in {0} ms)", sw.ElapsedMilliseconds);
                };

            // Parse some S-expr's from string representation
            // (single-line comments start with "//" and end on their line)
            var parsed =
                syntax.
                Parse
                (@"
                    ( let
                        (
                            ( id ( ( x ) => x ) )
                            ( factorial ( ( n ) => ( if ( > n 0 ) ( * n ( factorial ( - n 1 ) ) ) 1 ) ) )
                            ( f_o_g ( ( f g ) => ( ( x ) => ( f ( g x ) ) ) ) )

                            // See http://lambda-the-ultimate.org/node/5408
                            ( f ( ( x ) => ( ( g true ) , x ) ) )              // ML-ish: f x = (g True, x)
                            ( g ( ( b ) =>
                                ( if b
                                    0                                             // ML-ish: g True = 0
                                    ( + ( first ( f ""a"" ) ) ( first ( f 0 ) ) ) // ML-ish: g False = fst (f 'a') + fst (f 0)
                                )
                            ) )

                            // See (page 3)
                            // [Fritz Henglein] http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.388.430&rep=rep1&type=pdf
                            ( fmap (
                                ( f l ) =>
                                ( if ( ! ( empty l ) )
                                    ( ( f ( head l ) ) : ( fmap f ( tail l ) ) )
                                    ( )
                                )
                            ) )

                            // See (page 3, examples ""Double"", ""Mycroft"", ""Sum List"", ""Composition""...)
                            // [Hallett & Kfoury] http://itrs04.di.unito.it/papers/hk.pdf

                            // Double
                            ( double ( ( f ) => ( ( x ) => ( f ( f x ) ) ) ) )
                            ( foo ( ( n ) => ( ( double ( ( num ) => ( + num 1 ) ) ) n ) ) )
                            ( bar ( ( s ) => ( ( double escape ) s ) ) )

                            // Mycroft
                            ( sqList ( ( l ) => ( fmap ( ( x ) => ( * ( + x 0 ) x ) ) l ) ) )
                            ( compList ( ( l ) => ( fmap ! l ) ) )

                            // Sum List
                            ( sumList ( ( l ) =>
                                ( if ( empty l )
                                    0
                                    ( + ( id ( head l ) ) ( sumList ( id ( tail l ) ) ) )
                                )
                            ) )

                            // Isomorphic Compositions
                            ( createList ( ( x ) => ( x : ( ) ) ) )
                            ( removeList ( ( l ) => ( head l ) ) )
                            ( comp ( ( f g ) => ( f_o_g f g ) ) )
                            ( appComp ( ( v1 v2 ) =>
                                ( ==
                                    ( ( comp removeList createList ) v1 )
                                    ( head ( ( comp createList removeList ) v2 ) )
                                )
                            ) )
                            ( listOfTriples (
                                ( a b c ) => ( ( | a b c ) : ( ) )
                            ) )
                            ( pairOfLists (
                                ( a b ) => ( ( a : ( ) ) , ( b : ( ) ) )
                            ) )
                            ( testValue1 ( listOfTriples 1 ""a"" true ) )
                            ( testValue2 ( pairOfLists 1 ""a"" ) )
                        )

                        // report type inference results thru a fat tuple in the let's body
                        ( |
                            // (uncomment whatever you're interested in)
                            //( fmap ( f_o_g factorial parseInt ) ( ""0"" : ( ""1"" : ( ""2"" : ( ""3"" : ( ) ) ) ) ) )
                            //( first ( f true ) )
                            //( foo 123 )
                            //( bar ""z"" )
                            //( sqList ( 0 : ( 1 : ( 2 : ( 3 : ( ) ) ) ) ) )
                            //( compList ( true : ( false : ( ) ) ) )
                            //( sumList ( 1 : ( 2 : ( 3 : ( ) ) ) ) )
                            //( appComp 5 ( 5 : ( ) ) )
                            testValue1
                            testValue2
                        )
                    )
                ");

            var ast = visitSExpr(parsed);

            Console.WriteLine();
            Console.WriteLine("Abstract syntax:");
            foreach (var node in ast.Args)
            {
                Console.WriteLine();
                Console.WriteLine(node);
            }

            Console.WriteLine();
            Console.WriteLine("Inferred types:");

            // Visit the parsed S-expr, turn it into an AST for H-M type inference
            // (see Node, et al, above), infer some types from the latter,
            // and report
            analyze(ast);

            Console.ReadKey();
        }
    }
}

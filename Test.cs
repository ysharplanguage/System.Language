/*
Hindley-Milner type inference over (Lisp-ish) S-expressions (01/2017)

https://repl.it/FTwz/16

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
        public CtorInfo(string cons, int arity) { Cons = cons; Arity = arity; }
        public IType GetType(IEnvironment env) =>
            env.
            Values.
            FirstOrDefault
            (
                type =>
                    (type.Metadata != null) &&
                    (((CtorInfo)type.Metadata).Cons == Cons) &&
                    (
                        (((CtorInfo)type.Metadata).Arity == Arity) ||
                        (((CtorInfo)type.Metadata).Arity < 0) ||
                        (Arity < 0)
                    )
            );
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
                    Syntax.Lexical("[\\$_A-Za-z][\\$_0-9A-Za-z\\-\\.\\`]*", Syntax.NewSymbol),

                    // ... and such
                    Syntax.Lexical("[\\!\\&\\|\\<\\=\\>\\+\\-\\*\\/\\%\\,\\:@]", Syntax.NewSymbol),
                    Syntax.Lexical("\\&\\&", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\|\\|", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\<\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\>\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\!\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\=\\=", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\=\\>", Syntax.NewSymbol, true),
                    Syntax.Lexical("\\-\\>", Syntax.NewSymbol, true)
                );

            var system = new TypeSystem();
            var env = system.NewEnvironment();

            // Classic
            var @bool = system.NewType(typeof(bool).FullName, null);
            var @int = system.NewType(typeof(int).FullName, null);
            var @string = system.NewType(typeof(string).FullName, null);

            var SignatureType = system.NewType(TypeSystem.Function, null, new CtorInfo("->", -1));

            // Generic list type of some `item' type : List<item>;
            // syntax for lists:
            // <list-sexpr> ::= '(' <sexpr> ':' <list-sexpr> ')' | '(' ')'
            // (where "( )" is the empty list)
            var ListType = system.NewType("List", new[] { system.NewGeneric() }, new CtorInfo(":", 2));

            // Generic tuple types of some `item1', `item2', ... types : Tuple<item1, item2, ...>
            // syntax for tuples:
            // <tuple-sexpr> ::= '(' <sexpr> ',' ... ')' | '(' ',' ')'
            // (where "( , )" is the empty tuple and "( , <sexpr> )" or "( <sexpr> , )" the unary tuple)
            var Tuple00Type = system.NewType("Tuple`0", null, new CtorInfo(",", 0));
            var Tuple01Type = system.NewType("Tuple`1", new[] { system.NewGeneric() }, new CtorInfo(",", 1));
            var Tuple02Type = system.NewType("Tuple`2", new[] { system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 2));
            var Tuple03Type = system.NewType("Tuple`3", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 3));
            var Tuple04Type = system.NewType("Tuple`4", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 4));
            var Tuple05Type = system.NewType("Tuple`5", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 5));
            var Tuple06Type = system.NewType("Tuple`6", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 6));
            var Tuple07Type = system.NewType("Tuple`7", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 7));
            var Tuple08Type = system.NewType("Tuple`8", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 8));
            var Tuple09Type = system.NewType("Tuple`9", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 9));
            var Tuple10Type = system.NewType("Tuple`10", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 10));
            var Tuple11Type = system.NewType("Tuple`11", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 11));
            var Tuple12Type = system.NewType("Tuple`12", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 12));
            var Tuple13Type = system.NewType("Tuple`13", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 13));
            var Tuple14Type = system.NewType("Tuple`14", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 14));
            var Tuple15Type = system.NewType("Tuple`15", new[] { system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric(), system.NewGeneric() }, new CtorInfo(",", 15));

            // Populate the top level typing environment
            env["bool"] = env[@bool.Id] = @bool;
            env["int"] = env[@int.Id] = @int;
            env["string"] = env[@string.Id] = @string;
            env[SignatureType.Id] = SignatureType;
            env[ListType.Id] = ListType;
            new[]
            {
                Tuple00Type, Tuple01Type, Tuple02Type, Tuple03Type,
                Tuple04Type, Tuple05Type, Tuple06Type, Tuple07Type,
                Tuple08Type, Tuple09Type, Tuple10Type, Tuple11Type,
                Tuple12Type, Tuple13Type, Tuple14Type, Tuple15Type
            }.
            Aggregate(env, (e, t) => { e[t.Id] = t; return e; });

            // Bake some operator function types (to have something to infer about in familiar expressions)
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

            system.Infer(env, Node.Define(Node.Var("first"), Node.Abstract(new[] { Node.Var("pair", Tuple02Type) }, Tuple02Type[0], Node.Const(Tuple02Type[0]))));
            system.Infer(env, Node.Define(Node.Var("second"), Node.Abstract(new[] { Node.Var("pair", Tuple02Type) }, Tuple02Type[1], Node.Const(Tuple02Type[1]))));
            system.Infer(env, Node.Define(Node.Var("count"), Node.Abstract(new[] { Node.Var("list", ListType) }, @int, Node.Const(@int))));
            system.Infer(env, Node.Define(Node.Var("empty"), Node.Abstract(new[] { Node.Var("list", ListType) }, @bool, Node.Const(@bool))));
            system.Infer(env, Node.Define(Node.Var("head"), Node.Abstract(new[] { Node.Var("list", ListType) }, ListType[0], Node.Const(ListType[0]))));
            system.Infer(env, Node.Define(Node.Var("tail"), Node.Abstract(new[] { Node.Var("list", ListType) }, ListType, Node.Const(ListType))));
            system.Infer(env, Node.Define(Node.Var("length"), Node.Abstract(new[] { Node.Var("string", @string) }, @int, Node.Const(@int))));
            system.Infer(env, Node.Define(Node.Var("escape"), Node.Abstract(new[] { Node.Var("string", @string) }, @string, Node.Const(@string))));
            system.Infer(env, Node.Define(Node.Var("toString"), Node.Abstract(new[] { Node.Var("value", system.NewGeneric()) }, @string, Node.Const(@string))));
            system.Infer(env, Node.Define(Node.Var("parseInt"), Node.Abstract(new[] { Node.Var("string", @string) }, @int, Node.Const(@int))));

            // DRY helpers
            var isFunction = null as Func<IType, bool>;
            isFunction = type => type != null ? (type.Constructor != null ? type.Constructor == TypeSystem.Function : isFunction(type.Self)) : false;
            Func<string, int, IType> @typeof = (id, arity) => id != null ? (new CtorInfo(id, arity).GetType(env) ?? (env.ContainsKey(id) && env[id].Value.Name.IsConstructor ? env[id].Value : null)) : null;
            Func<string, int, bool> isCtor = (id, arity) => @typeof(id, arity) != null;
            Func<object, bool> isArray = value => value is object[];
            Func<object, object[]> array = value => (object[])value;
            Func<object, bool> isNil = value => isArray(value) && (array(value).Length == 0);

            // A sort of poor man's visitor (over the S-expr) : just a bunch of lambdas
            Func<IEnvironment, string, Node> visitError = (scope, message) => { throw new InvalidOperationException(message); };
            Func<IEnvironment, object, bool, Node> visitSExpr = null;
            Func<IEnvironment, object, Let> visitLet = null;
            Func<IEnvironment, object, int, bool, Define> visitDefine = null;
            Func<IEnvironment, object, bool, Apply> visitApply = null;
            Func<IEnvironment, object, Abstract> visitAbstract = null;
            Func<IEnvironment, object, int, bool, Apply> visitCtor = null;
            Func<IEnvironment, object, Var> visitVar = null;
            Func<IEnvironment, object, Const> visitConst = null;
            Func<IEnvironment, object, Node> visit;

            visitSExpr =
                (scope, sexpr, inType) =>
                {
                    var arity = (isArray(sexpr) ? array(sexpr).Length : 0) - 1;
                    var node =
                        isArray(sexpr) ?
                        (
                            // ( let / | / sexpr ... )
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
                                            visitApply(scope, sexpr, inType)
                                            :
                                            // ( sexpr <ctor> ... )
                                            visitCtor(scope, sexpr, 1, inType)
                                        )
                                        :
                                        // ( <ctor> ... )
                                        visitCtor(scope, sexpr, 0, inType)
                                    )
                                    :
                                    // ( ... => ... )
                                    !inType ? visitAbstract(scope, sexpr) : visitError(scope, "'=>' not allowed in type annotation")
                                )
                                :
                                // ( let ... )
                                !inType ? visitLet(scope, sexpr) : visitError(scope, "'let' not allowed in type annotation")
                            )
                            :
                            (
                                // ( id / sexpr )
                                !isNil(sexpr) ?
                                (
                                    !isCtor(Syntax.Identifier(array(sexpr)[0]), 0) ?
                                    // ( sexpr )
                                    (Node)visitApply(scope, sexpr, inType)
                                    :
                                    // ( <ctor> )
                                    visitCtor(scope, sexpr, 0, inType)
                                )
                                :
                                // ( )
                                visitConst(scope, ListType)
                            )
                        )
                        :
                        // id / const
                        Syntax.Identifier(sexpr) != null ? (Node)visitVar(scope, sexpr) : visitConst(scope, sexpr);
                    return node;
                };

            visitLet =
                (scope, sexpr) =>
                {
                    var let = Node.Let(array(array(sexpr)[1]).Select(item => visitDefine(scope, item, 0, false)).ToArray(), visitSExpr(scope, array(sexpr)[2], false));
                    return let;
                };

            visitDefine =
                (scope, sexpr, at, inType) =>
                {
                    var left = array(sexpr)[0];
                    Var identifier;
                    Node definition;
                    if (!inType && isArray(left) && (array(left).Length == 1))
                    {
                        var right = array(sexpr)[1];
                        var arity = (isArray(right) ? array(right).Length : 0) - 1;
                        identifier = visitVar(scope, array(left)[0]);
                        definition =
                            isArray(right) ?
                            (
                                array(right).Length > 1 ?
                                (
                                    !isCtor(Syntax.Identifier(array(right)[1]), arity) ?
                                    (
                                        !isCtor(Syntax.Identifier(array(right)[0]), arity) ?
                                        visitApply(scope, right, true)
                                        :
                                        visitCtor(scope, right, 0, true)
                                    )
                                    :
                                    visitCtor(scope, right, 1, true)
                                )
                                :
                                (
                                    !isNil(right) ?
                                    (
                                        !isCtor(Syntax.Identifier(array(right)[0]), 0) ?
                                        (Node)visitApply(scope, right, inType)
                                        :
                                        visitCtor(scope, right, 0, true)
                                    )
                                    :
                                    visitConst(scope, ListType)
                                )
                            )
                            :
                            Syntax.Identifier(right) != null ? (Node)visitVar(scope, right) : visitConst(scope, right);
                    }
                    else
                    {
                        identifier = visitVar(scope, array(sexpr)[at]);
                        definition = visitSExpr(scope, array(sexpr)[1 - at], inType);
                    }
                    var define = Node.Define(identifier, definition);
                    return define;
                };

            visitApply =
                (scope, sexpr, inType) =>
                {
                    Func<IType[], IType> newFunction = signature => system.NewType(TypeSystem.Function, signature);
                    Apply apply;
                    string id;
                    if (((id = Syntax.Identifier(array(sexpr)[0])) != null) && (!env.ContainsKey(id) || isFunction(env[id])))
                    {
                        var fn = Node.Var(id, !env.ContainsKey(id) ? env[id] = newFunction(array(sexpr).Skip(1).Select(arg => system.NewGeneric()).Concat(new[] { system.NewGeneric() }).ToArray()) : env[id]);
                        apply = Node.Apply(fn, array(sexpr).Skip(1).Select(arg => visitSExpr(scope, arg, false)).ToArray());
                    }
                    else
                    {
                        apply =
                            Node.Apply
                            (
                                visitSExpr(scope, array(sexpr)[0], inType),
                                array(sexpr).Skip(1).Select(arg => visitSExpr(scope, arg, inType)).ToArray(),
                                id != null ? env[id] : null,
                                inType
                            );
                    }
                    return apply;
                };

            visitAbstract =
                (scope, sexpr) =>
                {
                    Abstract lambda;
                    lambda =
                        Node.Abstract
                        (
                            array(array(sexpr)[0]).Select(arg => isArray(arg) ? (Node)visitDefine(scope, arg, 1, true) : visitVar(scope, arg)).ToArray(),
                            visitSExpr(scope, array(sexpr)[2], false)
                        );
                    return lambda;
                };

            visitCtor =
                (scope, sexpr, at, inType) =>
                {
                    var identifier = Syntax.Identifier(array(sexpr)[at]);
                    var isList = identifier == ((CtorInfo)ListType.Metadata).Cons;
                    var arity = at > 0 ? (array(sexpr).Length + 1) / 2 : array(sexpr).Length - 1;
                    var type = @typeof(identifier, arity);
                    var argv =
                        at > 0 ?
                        array(sexpr).Where((arg, i) => (i % 2) == 0).Select(arg => visitSExpr(scope, arg, inType)).Take(isList ? arity - 1 : arity).ToArray()
                        :
                        array(sexpr).Skip(1).Select(arg => visitSExpr(scope, arg, inType)).Take(isList ? arity - 1 : arity).ToArray();
                    var ctor = Node.Apply(Node.Var(type.Id), argv, isFunction(type) ? TypeSystem.Function : type, inType);
                    return ctor;
                };

            visitVar =
                (scope, sexpr) =>
                {
                    var var = Node.Var(Syntax.Identifier(sexpr));
                    return var;
                };

            visitConst =
                (scope, sexpr) =>
                {
                    var @const = Node.Const(sexpr);
                    return @const;
                };

            visit =
                (scope, sexpr) =>
                {
                    var root = visitSExpr(scope, sexpr, false);
                    return root;
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
                                return system.NewType(string.Format("Type error: {0}", ex.Message), null);
                            }
                        };
                    var sw = new System.Diagnostics.Stopwatch();
                    sw.Start();
                    foreach (var node in root.Args)
                    {
                        Console.WriteLine();
                        Console.WriteLine(node.Spec is Node ? string.Format("{0}:\r\n\t{1}", (Node)node.Spec, tryInfer(node).Value) : tryInfer(node).Value.ToString());
                    }
                    sw.Stop();
                    Console.WriteLine();
                    Console.WriteLine(string.Format("{0}:\r\n\t{1}", root.Body, tryInfer(root.Body)));
                    Console.WriteLine();
                    Console.WriteLine("... Done (in {0} ms)", sw.ElapsedMilliseconds);
                    Console.WriteLine();
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
                            ( f ( ( x ) => ( ( g true ) , x ) ) )                 // ML-ish: f x = (g True, x)

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

                            ( threeValuesToAListOfTriples (
                                ( a b c ) => ( ( a , b , c ) : ( ) )
                            ) )
                            ( twoValuesToAPairOfLists (
                                ( a b ) => ( ( a : ( ) ) , ( b : ( ) ) )
                            ) )

                            // demonstrates type annotations
                            ( ( Pair ) ( a -> b -> ( ( a , b ) this ) ) ) // ( Pair a b ) just an alias for ( Tuple`2 a b )
                            
                            ( ( Atoi ) ( string -> int ) ) // string to integer converter signature

                            // should be inferred as,
                            // atoiTest : ( string -> int ) -> ( List string ) -> ( List ( string , int ) )
                            ( atoiTest (
                                (
                                    ( Atoi convert ) // (the converter)
                                    l // (some list)
                                )
                                => ( ( ( head l ) , ( convert ( head l ) ) ) : ( atoiTest convert ( tail l ) ) )
                            ) )

                            ( ( Triples ) ( x -> y -> z -> ( ( ( ( x , y , z ) triple ) : ( ) ) this ) ) )
                            ( Cloud ( ( v ) => ( Triples v v v ) ) )
                            ( number-cloud ( Cloud 0 ) )
                            ( string-cloud ( Cloud """" ) )

                            // advanced / more interesting:
                            // demonstrates generic type annotations
                            // mixed with constructor syntax ...
                            ( moreInteresting (
                                (
                                    // (`f' takes an `a' and returns a pair of `a' and `b')
                                    ( ( a -> ( ( a , b ) p ) ) f )
                                    l
                                ) =>
                                // `l' will be inferred to inhabit `( List a )' ...
                                // ... and as for the result of moreInteresting,
                                // to inhabit `( List b )'
                                ( ( second ( f ( head l ) ) ) : ( ) )
                            ) )

                            ( testOutput1 ( threeValuesToAListOfTriples 1 ""a"" true ) )
                            ( testOutput2 ( twoValuesToAPairOfLists 1 ""a"" ) )                            

                            // moreInteresting which will typecheck
                            ( testOutput3 (
                                moreInteresting
                                ( ( s ) => ( s , ( parseInt s ) ) ) // ... that's for the formal `f' (a lambda)
                                ( ""1"" : ( ""2"" : ( ) ) ) // ... that's for the formal `l'
                            ) )
                            // moreInteresting which cannot typecheck
                            ( testOutput3CannotTypecheck (
                                moreInteresting
                                ( ( s ) => ( s , ( parseInt s ) ) )
                                ( 1 : ( 2 : ( ) ) )
                            ) )
                        )

                        // report type inference results thru a fat tuple in let's body
                        //( uncomment whatever you're interested in )
                        (
                            ( id ( 0 , ""1"", true ) )
                            //, ( fmap ( f_o_g factorial parseInt ) ( ""0"" : ( ""1"" : ( ""2"" : ( ""3"" : ( ) ) ) ) ) )
                            //, ( first ( f true ) )
                            //, ( foo 123 )
                            //, ( bar ""z"" )
                            //, ( sqList ( 0 : ( 1 : ( 2 : ( 3 : ( ) ) ) ) ) )
                            //, ( compList ( true : ( false : ( ) ) ) )
                            //, ( sumList ( 1 : ( 2 : ( 3 : ( ) ) ) ) )
                            //, ( appComp 5 ( 5 : ( ) ) )
                            , testOutput1
                            , testOutput2
                            , testOutput3
                            , testOutput3CannotTypecheck
                        )
                    )
                ");

            var ast = visit(env, parsed);

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

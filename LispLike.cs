/*
Lisp-ish interpreter ( https://en.wikipedia.org/wiki/Lisp_%28programming_language%29 )

Copyright (c) 2017 Cyril Jandia

https://ysharp.io/

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

namespace LispLike
{
    using System.Language.SExpressions;

    public class Lispish : Language
    {
        internal class EqualityComparer : IEqualityComparer<object>
        {
            internal bool Equal(object x, object y)
            {
                return
                    Equals(x, y) ||
                    (
                        (x != null) &&
                        (y != null) &&
                        (
                            (x is object[]) &&
                            (y is object[]) &&
                            ((object[])x).SequenceEqual((object[])y, this)
                        )
                    );
            }

            bool IEqualityComparer<object>.Equals(object x, object y)
            {
                return Equal(x, y);
            }

            int IEqualityComparer<object>.GetHashCode(object obj)
            {
                return obj != null ? obj.GetHashCode() : 0;
            }
        }

        internal static readonly EqualityComparer Equality = new EqualityComparer();

        // demoes various features of the System.Language.SExpressions namespace
        public Lispish()
            : base()
        {
            Syntax.Include
            (
                // syntactic core
                Syntax.Lexical("\\/\\/.*", Syntax.Commenting), // comment
                Syntax.Token("@", Syntax.NewSymbol), // apply
                Syntax.Token("'", Syntax.Quoting), // quote
                Syntax.Token(":", Syntax.NewSymbol), // cons (list construction)
                Syntax.Token("=", Syntax.NewSymbol), // let symbol
                Syntax.Token("=>", Syntax.NewSymbol, true), // lambda symbol, will have priority to match before '='
                Syntax.Token("?", Syntax.NewSymbol), // test (special form, that can be unary, binary, or ternary)
                Syntax.Lexical("params", Syntax.Parameters), // params (actually, a special form: equiv. of JavaScript's "arguments")
                Syntax.Lexical("this", Syntax.Self), // this (the current closure)

                // extensions
                Syntax.Token(".", Syntax.NewSymbol), // dot (will be used to simulate member access in concrete syntax)
                Syntax.Token("...", Syntax.NewSymbol, true), // list, has priority to match before '.'

                // builtins
                Syntax.Token("#", Syntax.NewSymbol), // n-th item (array / list -indexed access)
                Syntax.Lexical("[0-9]+", (token, match) => int.Parse(match)),
                Syntax.Lexical("\\\"(\\\\\\n|\\\\t|\\\\n|\\\\r|\\\\\\\"|[^\\\"])*\\\"", (token, match) => match.Substring(1, match.Length - 2)),

                // identifier, has priority to match before "params", etc
                Syntax.Lexical("[\\$_A-Za-z][\\$_0-9A-Za-z\\-\\!]*", Syntax.NewSymbol, true),

                Syntax.Lexical("\\!\\=", Syntax.NewSymbol),
                Syntax.Lexical("\\=\\=", Syntax.NewSymbol, true), // has priority to match before '='
                Syntax.Lexical("\\&\\&", Syntax.NewSymbol),
                Syntax.Lexical("\\|\\|", Syntax.NewSymbol),
                Syntax.Lexical("\\!", Syntax.NewSymbol),
                Syntax.Lexical("[\\+\\-\\*\\/\\%]", Syntax.NewSymbol),

                Syntax.Lexical("\\<\\=", Syntax.NewSymbol),
                Syntax.Lexical("\\>\\=", Syntax.NewSymbol),
                Syntax.Lexical("\\<", Syntax.NewSymbol),
                Syntax.Lexical("\\>", Syntax.NewSymbol)
            );

            Include
            (
                // semantic core (special forms)
                Keyword("@", Application, true), // will support infix notation
                Keyword("'", Quoting),
                Keyword(":", Construction, true), // will support infix notation
                Keyword("=", Letting),
                Keyword("=>", Abstraction, true), // will support infix notation
                Keyword("?", Testing, true)
            );
        }

        // demoes how builtins can be retrieved thru reflection
        [Keyword(".", true)]
        public object dot(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            if (list.Length > 1)
            {
                return Evaluation(environment, new[] { list[1], list[0] }.Concat(list.Skip(2)).ToArray());
            }
            throw Error("list's length must be greater than one");
        }

        [Keyword("...")]
        public object list(Environment environment, object expression)
        {
            var list = expression as object[];
            return list != null ? list.Select(item => Evaluation(environment, item)).ToArray() : new object[0];
        }

        [Builtin("!=")]
        public bool notEqual(Environment environment, object left, object right) => !equal(environment, left, right);

        [Builtin("==")]
        public bool equal(Environment environment, object left, object right) => Equality.Equal(left, right);

        [Builtin("&&")]
        public bool and(Environment environment, bool left, bool right) => left && right;

        [Builtin("||")]
        public bool or(Environment environment, bool left, bool right) => left || right;

        [Builtin("!")]
        public bool not(Environment environment, bool value) => !value;

        [Builtin("+")]
        public int add(Environment environment, int left, int right) => left + right;

        [Builtin("-")]
        public int subtract(Environment environment, int left, int right) => left - right;

        [Builtin("*")]
        public int multiply(Environment environment, int left, int right) => left * right;

        [Builtin("/")]
        public int divide(Environment environment, int left, int right) => left / right;

        [Builtin("%")]
        public int modulo(Environment environment, int left, int right) => left % right;

        [Builtin("<=")]
        public bool le(Environment environment, int left, int right) => left <= right;

        [Builtin(">=")]
        public bool ge(Environment environment, int left, int right) => left >= right;

        [Builtin("<")]
        public bool lt(Environment environment, int left, int right) => left < right;

        [Builtin(">")]
        public bool gt(Environment environment, int left, int right) => left > right;

        // builtins can also be defined thru read only properties
        [Builtin("void")]
        public Type @void { get; } = typeof(void);

        // ... or read only fields
        [Builtin("null")]
        public readonly object @null = null;

        [Builtin("true")]
        public readonly bool @true = true;

        [Builtin("false")]
        public readonly bool @false = false;

        [Builtin("typeof")]
        public Type @typeof(Environment environment, object value) => value != null ? value.GetType() : @void;

        [Builtin("string")]
        public string @string(Environment environment, object value) => Syntax.ToString(value);

        [Builtin("format")]
        public string format(Environment environment, string template, object[] arguments) => (arguments != null) && (arguments.Length > 0) ? string.Format(template, arguments.Select(argument => Syntax.ToString(argument)).ToArray()) : template;

        [Builtin("concat")]
        public object[] concat(Environment environment, params object[] arguments) => arguments.SelectMany((object list) => (object[])list).ToArray();
    }

    class Program
    {
        static void Main(string[] args)
        {
            var language = new Lispish();
            language.
                // more builtins;
                // this time, defined programmatically
                // by mutating the language definition instance
                Include
                (
                    Language.Builtin("write", (Environment environment, object value) => { Console.Write(value = language.format(environment, ((string)(value = Syntax.ToString(value))).StartsWith("\"") && ((string)value).EndsWith("\"") ? ((string)value).Substring(1, ((string)value).Length - 2) : (string)value, null)); return value; }),
                    Language.Builtin("writeln", (Environment environment, object value) => { Console.WriteLine(value = language.format(environment, ((string)(value = Syntax.ToString(value))).StartsWith("\"") && ((string)value).EndsWith("\"") ? ((string)value).Substring(1, ((string)value).Length - 2) : (string)value, null)); return value; }),

                    Language.Builtin("#", (Environment environment, object[] list, int index) => list[index]),
                    Language.Builtin("empty", (Environment environment, object value) =>
                        !(value is Callable) ?
                        (
                            !(value is object[]) ?
                            (
                                value is string ?
                                ((string)value).Length == 0
                                :
                                false
                            )
                            :
                            ((object[])value).Length == 0
                        )
                        :
                        ((Callable)value).Length == 0
                    ),
                    Language.Builtin("length", (Environment environment, object value) =>
                        !(value is Callable) ?
                        (
                            !(value is object[]) ?
                            (
                                value is string ?
                                ((string)value).Length
                                :
                                int.MinValue
                            )
                            :
                            ((object[])value).Length
                        )
                        :
                        ((Callable)value).Length
                    ),
                    Language.Builtin("where", (Environment environment, object[] list, Callable filter) =>
                        filter != null ?
                        (
                            filter.Length > 1 ?
                            (list != null ? list.Where((item, index) => (bool)filter.Invoke(environment, item, index)).ToArray() : new object[] { })
                            :
                            (list != null ? list.Where(item => (bool)filter.Invoke(environment, item)).ToArray() : new object[] { })
                        )
                        :
                        (list != null ? list : new object[] { })
                    ),
                    Language.Builtin("map", (Environment environment, object[] list, Callable mapper) => mapper != null ? (list != null ? list.Select(item => mapper.Invoke(environment, item)).ToArray() : new object[] { }) : (list != null ? list : new object[] { })),
                    Language.Builtin("reduce", (Environment environment, object[] list, object initial, Callable reducer, Callable mapper) =>
                        list != null ?
                        (
                            mapper != null ?
                            list.Aggregate(initial, (reduced, item) => reducer.Invoke(environment, reduced, item), reduced => mapper.Invoke(environment, reduced))
                            :
                            list.Aggregate(initial, (reduced, item) => reducer.Invoke(environment, reduced, item))
                        )
                        :
                        new object[] { }
                    ),
                    Language.Builtin("all", (Environment environment, object[] list, Callable predicate) => list != null ? (predicate != null ? (object)list.All(item => (bool)predicate.Invoke(environment, item)) : true) : true),
                    Language.Builtin("any", (Environment environment, object[] list, Callable predicate) => list != null ? (predicate != null ? (object)list.Any(item => (bool)predicate.Invoke(environment, item)) : list.Any()) : false),
                    Language.Builtin("contains", (Environment environment, object[] list, object value) => list != null ? (object)list.Contains(value, Lispish.Equality) : false),
                    Language.Builtin("count", (Environment environment, object[] list, Callable predicate) => list != null ? (predicate != null ? (object)list.Count(item => (bool)predicate.Invoke(environment, item)) : list.Count()) : 0),
                    Language.Builtin("distinct", (Environment environment, object[] list) => list != null ? list.Distinct(Lispish.Equality).ToArray() : new object[] { }),
                    Language.Builtin("add", (Environment environment, object[] list, object value) => list != null ? list.Concat(new[] { value }).ToArray() : null),
                    Language.Builtin("first", (Environment environment, object[] list, object countOrPredicate) =>
                        list != null ?
                        (
                            countOrPredicate != null ?
                            (
                                countOrPredicate is Callable ?
                                list.TakeWhile(item => (bool)((Callable)countOrPredicate).Invoke(environment, item)).ToArray()
                                :
                                list.Take((int)countOrPredicate).ToArray()
                            )
                            :
                            (countOrPredicate = list.FirstOrDefault()) != null ? new object[] { countOrPredicate } : new object[] { }
                        )
                        :
                        new object[] { }
                    ),
                    Language.Builtin("last", (Environment environment, object[] list, object countOrPredicate) => list != null ? environment.Invoke("first", list.Reverse().ToArray(), countOrPredicate) : new object[] { }),
                    Language.Builtin("remaining", (Environment environment, object[] list) => environment.Invoke("skip", list, 1)),
                    Language.Builtin("skip", (Environment environment, object[] list, object countOrPredicate) =>
                        list != null ?
                        (
                            countOrPredicate != null ?
                            (
                                countOrPredicate is Callable ?
                                list.SkipWhile(item => (bool)((Callable)countOrPredicate).Invoke(environment, item)).ToArray()
                                :
                                list.Skip((int)countOrPredicate).ToArray()
                            )
                            :
                            list
                        )
                        :
                        new object[] { }
                    )
                );

            language.
                Evaluate
                (@"
                    ( =     // ""="" is Scheme's let
                        (
                            ( a 123456 ) // a single line comment
                            ( b ""b"" )  
                            ( c ( ... 10 11 12 ) )
                            ( sets ( ( 1 2 3 ) ( 123 45 ) ( 456 1 2 3 ) ( 12 123 ) ( 1 2 3 4 ) ) )
                        )
                        (
                            ( writeln c )
                            ( writeln a )
                            ( writeln ( a : c ) )
                            ( writeln ( a : ( b : c ) ) )
                            ( writeln b )
                            ( writeln ( ... b ( 9 : c ) 13 ) )
                            ( writeln
                                (
                                    sets .where
                                    ( ( set ) => ( set .contains 123 ) )
                                )
                            )
                        )
                    )
                ");
            Console.WriteLine();

            // demoes the use of "." as infix, (pseudo) member access operator ("a la" OOP)
            language.Evaluate(@"( writeln ( ( 1 3 5 7 ) .contains 5 ) )"); // true
            language.Evaluate(@"( writeln ( ( 2 5 6 1 3 4 ) .where ( ( n ) => ( == ( % n 2 ) 0 ) ) ) )"); // true
            language.Evaluate(@"( writeln ( ( 2 5 6 14 8 ) .where ( ( n ) => ( == ( % n 7 ) 0 ) ) ) )"); // false
            language.Evaluate(@"( writeln ( distinct ( 1 3 2 5 2 5 7 6 ) ) )"); // ( 1 3 2 5 7 6 )
            Console.WriteLine();

            System.Diagnostics.Debug.Assert("123" == (string)language.
                Evaluate
                (@"
                    ( writeln (
                        ( ( a b c ) => (
                                + a ( + b c )
                            )
                        )
                        @ ( 100 20 3 )
                    ) )
                ")
            );
            Console.WriteLine();

            // demoes "params" (same as JavaScript's "arguments")
            System.Diagnostics.Debug.Assert("( 1 2 3 )" == (string)language.Evaluate(@"( writeln ( ( ( ) => params ) 1 2 3 ) )"));
            Console.WriteLine();

            System.Diagnostics.Debug.Assert("( 2 3 )" == (string)language.Evaluate(@"( writeln ( ( ( ) => ( remaining params ) ) @ ( 1 2 3 ) ) )"));
            Console.WriteLine();

            language.
                Evaluate
                (@"
                    ( writeln
                        ""a\tb\r\nc\t\td\ne\t\t\tf\
g\t\t\t\th\n\""i\""j\""k""
                    )
                ");
            Console.WriteLine();

            // demoes traditional Lisp / Scheme quotes
            System.Diagnostics.Debug.Assert("( x 1 y 2 z 3 )" == (string)language.
                Evaluate
                (@"
                    ( writeln ' ( x 1 y 2 z 3 ) )
                ")
            );
            Console.WriteLine();

            System.Diagnostics.Debug.Assert("( 123 )" == (string)language.
                Evaluate
                (@"
                    ( writeln (
                        (
                            ( ( ) => (
                                    ( ! ( params .empty ) ) ?
                                    ( + ( ( params .first ). #0 ) ( this @ ( remaining params ) ) )
                                    0
                                )
                            )
                            100 20 3
                        ) // apply
                    ) ) // writeln
                ")
            );
            Console.WriteLine();

            System.Diagnostics.Debug.Assert("( 200 400 )" == (string)language.
                Evaluate
                (@"
                    ( writeln
                            ( = (
                                    ( not-equal != )
                                )
                                    (   ( ( ) => ( params .where
                                                (
                                                    ( item index ) =>
                                                    ( ( % index 2 ). not-equal 0 )
                                                )
                                            )
                                        )
                                        100 200 300 400 500
                                    )
                            )
                    )
                ")
            );
            Console.WriteLine();

            // more traditional syntax (non-infixed operators)
            var bar = language.
                Evaluate
                (@"
                    ( string ( where ( 0 1 2 3 4 5 6 7 8 9 )
                        ( ( i ) => ( != ( % i 2 ) 0 ) )
                    ) )
                ");
            System.Diagnostics.Debug.Assert((string)bar == "( 1 3 5 7 9 )");
            Console.WriteLine(bar);
            Console.WriteLine();

            var factorial12 = language.
                Evaluate
                (@"
                        ( = (
                                ( arg ( 12 ) )
                                (
                                    n!
                                    (
                                        ( k ) => (
                                            ( != k 0 ) ?
                                            ( * k ( this ( - k 1 ) ) )
                                            1
                                        )
                                    )
                                )
                            )
                            ( n! @ arg )
                        )
                ");
            System.Diagnostics.Debug.Assert((int)factorial12 == 479001600);
            Console.WriteLine(factorial12);
            Console.WriteLine();

            var factorial = language.
                Evaluate
                (@"
                    ( ( k ) => (
                            ( != k 0 ) ?
                            ( * k ( this ( - k 1 ) ) )
                            1
                        )
                    )
                ");
            System.Diagnostics.Debug.Assert(factorial.ToString() == "( k ) => ( ( != k 0 ) ? ( * k ( this ( - k 1 ) ) ) 1 )");
            Console.WriteLine(factorial);
            Console.WriteLine();

            // host "interop" demo
            var sw = new System.Diagnostics.Stopwatch();
            var r = 0;
            sw.Start();
            for (var k = 1; k <= 10000; k++)
            {
                r = (int)((Lambda)factorial).Invoke(null, 12);
            }
            sw.Stop();
            System.Diagnostics.Debug.Assert(r == 479001600);
            Console.WriteLine(r);
            Console.WriteLine();
            Console.WriteLine("{0} ms", sw.ElapsedMilliseconds);
            Console.WriteLine();

            // advanced example: demoes how to overload a builtin (here, "+"), from within the language
            // to dispatch on its argument(s) run time types
            var @out = (object[])language.
                Evaluate
                (@"
                    ( =
                        (
                            (
                                +
                                ( ( left right ) => (
                                    (
                                        ( == ( typeof ( ) ) ( typeof left ) ). &&
                                        ( == ( typeof ( ) ) ( typeof right ) )
                                    ) ?
                                    ( left .concat right )
                                    ( left. + right )
                                ) )
                            )
                        )
                        (
                            ( writeln ( + 1 2 ) )
                            ( writeln """" )
                            ( writeln ( ""{0}, {1}, {2}"" .format ( 1 2 3 ) ) )
                            ( writeln """" )
                            ( writeln ( ( 3 4 ). + ( 1 2 ) ) )
                            ( writeln """" )
                            ( writeln ( + ( 1 ( - 7 5 ) ) ( 3 4 ) ) )
                            ( writeln """" )
                            ( writeln ( + .string ) )
                        )
                    )
                ");
            System.Diagnostics.Debug.Assert((string)@out[2] == "1, 2, 3");
            System.Diagnostics.Debug.Assert((string)@out[4] == "( 3 4 1 2 )");
            System.Diagnostics.Debug.Assert((string)@out[6] == "( 1 2 3 4 )");
            System.Diagnostics.Debug.Assert((string)@out[8] == "( left right ) => ( ( ( == ( typeof ( ) ) ( typeof left ) ) . && ( == ( typeof ( ) ) ( typeof right ) ) ) ? ( left . concat right ) ( left . + right ) )");

            Console.WriteLine("The end.");
            Console.ReadKey();
        }
    }
}

/*
System.Language.SExpressions ( https://en.wikipedia.org/wiki/S-expression )

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
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

namespace System.Language.SExpressions
{
    public delegate object Acceptor(Token token, string match);

    public class Symbol
    {
        public Symbol(string id) { Id = id ?? Guid.NewGuid().ToString("P"); }
        public override string ToString() => Id;
        public string Id { get; private set; }
    }

    public class Token : Symbol
    {
        internal Token(string id) : base(id) { }
        public Token(string id, Acceptor acceptor) : this(id, acceptor, false) { }
        public Token(string id, Acceptor acceptor, bool priority) : this(id, id, acceptor, priority) { }
        public Token(string id, string pattern, Acceptor acceptor) : this(id, pattern, acceptor, false) { }
        public Token(string id, string pattern, Acceptor acceptor, bool priority) : base(id) { Regex = new Regex(string.Format("^({0})", !string.IsNullOrEmpty(Pattern = pattern) ? Pattern : ".*"), RegexOptions.Compiled); ValueOf = acceptor; HasPriority = priority; }
        public string Pattern { get; private set; }
        public Regex Regex { get; private set; }
        public Acceptor ValueOf { get; private set; }
        public bool HasPriority { get; private set; }
    }

    public class Syntax
    {
        private static readonly Token Space = Token("s+", Echo, 1);
        private static readonly Token Open = Token("(", Echo);
        private static readonly Token Close = Token(")", Echo);
        private static object Echo(Token token, string match) => new Token(token.Id);
        private Token comment;
        private Token quote;
        private Token @params;
        private Token @this;

        private Tuple<Token, string, object> Read(ref string input)
        {
            if (!string.IsNullOrEmpty(input))
            {
                var found = null as Match;
                var sofar = input;
                var tuple = Lexicon.FirstOrDefault(current => (found = current.Item2.Regex.Match(sofar)).Success && (found.Length > 0));
                var token = tuple != null ? tuple.Item2 : null;
                var match = token != null ? found.Value : null;
                input = match != null ? input.Substring(match.Length) : input;
                return token != null ? Tuple.Create(token, match, token.ValueOf(token, match)) : null;
            }
            return null;
        }

        private Tuple<Token, string, object> Next(ref string input)
        {
            Tuple<Token, string, object> read;
            while (((read = Read(ref input)) != null) && ((read.Item1 == Comment) || (read.Item1 == Space))) ;
            return read;
        }

        public object Parse(ref string input, Tuple<Token, string, object> next)
        {
            var value = null as object;
            if (next != null)
            {
                var token = next.Item1;
                if (token == Open)
                {
                    var list = new List<object>();
                    while (((next = Next(ref input)) != null) && (next.Item1 != Close))
                    {
                        list.Add(Parse(ref input, next));
                    }
                    if (next == null)
                    {
                        throw Language.Error("unexpected EOF");
                    }
                    value = list.ToArray();
                }
                else if (token == Quote)
                {
                    var quote = next.Item3;
                    next = Next(ref input);
                    if (next == null)
                    {
                        throw Language.Error("unexpected EOF");
                    }
                    value = new[] { quote, Parse(ref input, next) };
                }
                else
                {
                    value = next.Item3;
                }
            }
            else
            {
                throw Language.Error("unexpected EOF");
            }
            return value;
        }

        protected Token TokenOf(Acceptor acceptor)
        {
            var optional = new Acceptor[] { Commenting, Parameters, Self };
            var found = Lexicon.FirstOrDefault(pair => pair.Item2.ValueOf == acceptor);
            var token = found != null ? found.Item2 : null;
            if ((token == null) && !Optional.Contains(acceptor))
            {
                throw Language.Error("missing required token definition: {0}", acceptor.Method.Name);
            }
            return token;
        }

        protected virtual Syntax Validate()
        {
            Required.Select(acceptor => TokenOf(acceptor)).Count();
            return this;
        }

        protected IList<Tuple<string, Token>> Lexicon { get; private set; }
        protected HashSet<Acceptor> Required { get; set; }
        protected HashSet<Acceptor> Optional { get; set; }

        public static Symbol Symbol(object value) => value as Symbol;
        public static string Identifier(object value) => Symbol(value) != null ? Symbol(value).Id : null;
        public static string Escape(string pattern) => Escape(pattern, -1);

        public static string Escape(string pattern, int count)
        {
            count = count < 0 ? pattern.Length : count;
            return pattern.Aggregate(new StringBuilder(), (escaped, character) => escaped.AppendFormat("{0}{1}", count-- > 0 ? "\\" : string.Empty, character)).ToString();
        }

        public static object Commenting(Token token, string match) => Echo(token, match);
        public static object NewSymbol(Token token, string match) => new Symbol(match);
        public static object Quoting(Token token, string match) => NewSymbol(token, match);
        public static object Parameters(Token token, string match) => NewSymbol(token, match);
        public static object Self(Token token, string match) => NewSymbol(token, match);
        public static Token Token(string pattern, Acceptor acceptor) => Token(pattern, acceptor, -1);
        public static Token Token(string pattern, Acceptor acceptor, int escapeCount) => Token(pattern, acceptor, escapeCount, false);
        public static Token Token(string pattern, Acceptor acceptor, bool hasPriority) => Token(pattern, acceptor, -1, hasPriority);
        public static Token Token(string pattern, Acceptor acceptor, int escapeCount, bool hasPriority) => new Token(pattern, Escape(pattern, escapeCount), acceptor, hasPriority);
        public static Token Lexical(string pattern, Acceptor acceptor) => Token(pattern, acceptor, 0);
        public static Token Lexical(string pattern, Acceptor acceptor, bool hasPriority) => Token(pattern, acceptor, 0, hasPriority);

        public static string ToString(object value)
        {
            return
                value is object[] ?
                ((object[])value).Length > 0 ? ((object[])value).Aggregate(new StringBuilder("("), (result, obj) => result.AppendFormat(" {0}", ToString(obj))).Append(" )").ToString() : "( )"
                :
                (value != null ? (value is string ? string.Concat('"', (string)value, '"') : (value is bool ? value.ToString().ToLower() : value.ToString())).Replace("\\\r\n", "\r\n").Replace("\\\n", "\n").Replace("\\t", "\t").Replace("\\n", "\n").Replace("\\r", "\r").Replace("\\\"", "\"") : null) ?? "(null)";
        }

        public Syntax()
        {
            Lexicon = new List<Tuple<string, Token>>();
            Required = new HashSet<Acceptor>(new Acceptor[] { NewSymbol, Quoting });
            Optional = new HashSet<Acceptor>(new Acceptor[] { Commenting, Parameters, Self });
            Include(Space, Open, Close);
        }

        public Syntax Include(Token token)
        {
            if (Lexicon.FirstOrDefault(pair => pair.Item1 == token.Pattern) == null)
            {
                if (token.HasPriority)
                {
                    Lexicon.Insert(0, Tuple.Create(token.Pattern, token));
                }
                else
                {
                    Lexicon.Add(Tuple.Create(token.Pattern, token));
                }
            }
            return this;
        }

        public Syntax Include(params Token[] tokens) => tokens.Aggregate(this, (syntax, token) => syntax.Include(token));

        public object Parse(string input)
        {
            Validate();
            var next = Next(ref input);
            var value = Parse(ref input, next);
            if ((next = Next(ref input)) != null)
            {
                throw Language.Error("unexpected ", next.Item1);
            }
            return value;
        }

        public Token Comment { get { return comment = comment ?? TokenOf(Commenting); } }
        public Token Quote { get { return quote = quote ?? TokenOf(Quoting); } }
        public Token Params { get { return @params = @params ?? TokenOf(Parameters); } }
        public Token This { get { return @this = @this ?? TokenOf(Self); } }
    }

    public class SemanticAttribute : Attribute
    {
        protected SemanticAttribute(string id) { Id = id; }
        public string Id { get; private set; }
    }

    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Method | AttributeTargets.Field, AllowMultiple = false, Inherited = false)]
    public class BuiltinAttribute : SemanticAttribute
    {
        public BuiltinAttribute(string id) : base(id) { }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = false, Inherited = false)]
    public class KeywordAttribute : SemanticAttribute
    {
        public KeywordAttribute(string id) : this(id, false) { }
        public KeywordAttribute(string id, bool isInfix) : base(id) { IsInfix = isInfix; }
        public bool IsInfix { get; private set; }
    }

    public class Callable : Symbol
    {
        protected Callable(int length) : this(null, length) { }
        protected Callable(string id, int length) : base(id) { Length = length; }
        protected int Count(object[] arguments) { return arguments != null ? arguments.Length : 0; }
        public virtual object Invoke(params object[] arguments) { throw Language.Error("not implemented"); }
        public Delegate Delegate { get; protected set; }
        public bool Variadic { get; protected set; }
        public int Length { get; private set; }
    }

    public delegate object Semantic(Environment environment, object expression);

    public class Builtin : Callable
    {
        protected Builtin(string id, int length) : base(id, length) { }
    }

    public class Keyword : Builtin
    {
        public Keyword(string id, Semantic semantic) : this(id, semantic, false) { }
        public Keyword(string id, Semantic semantic, bool isInfix) : base(id, -1) { Delegate = semantic; IsInfix = isInfix; }
        public Semantic Semantic { get { return (Semantic)Delegate; } }
        public bool IsInfix { get; private set; }
    }

    public class Builtin<TResult> : Builtin
    {
        public Builtin(string id, Func<Environment, TResult> function) : base(id, 0) { Delegate = (Function = function); }
        public override object Invoke(params object[] arguments) { var count = Count(arguments); return Function(count > 0 ? (Environment)arguments[0] : null); }
        public Func<Environment, TResult> Function { get; private set; }
    }

    public class Builtin<T1, TResult> : Builtin
    {
        public Builtin(string id, Func<Environment, T1, TResult> function) : base(id, 1) { Delegate = (Function = function); var arg1 = Function.Method.GetParameters()[1]; Variadic = (typeof(T1) == typeof(object[])) && (arg1.GetCustomAttributes(false).FirstOrDefault(attribute => attribute is ParamArrayAttribute) != null); }
        public override object Invoke(params object[] arguments) { var count = Count(arguments); return Function(count > 0 ? (Environment)arguments[0] : null, count > 1 ? (T1)arguments[1] : default(T1)); }
        public Func<Environment, T1, TResult> Function { get; private set; }
    }

    public class Builtin<T1, T2, TResult> : Builtin
    {
        public Builtin(string id, Func<Environment, T1, T2, TResult> function) : base(id, 2) { Delegate = (Function = function); }
        public override object Invoke(params object[] arguments) { var count = Count(arguments); return Function(count > 0 ? (Environment)arguments[0] : null, count > 1 ? (T1)arguments[1] : default(T1), count > 2 ? (T2)arguments[2] : default(T2)); }
        public Func<Environment, T1, T2, TResult> Function { get; private set; }
    }

    public class Builtin<T1, T2, T3, TResult> : Builtin
    {
        public Builtin(string id, Func<Environment, T1, T2, T3, TResult> function) : base(id, 3) { Delegate = (Function = function); }
        public override object Invoke(params object[] arguments) { var count = Count(arguments); return Function(count > 0 ? (Environment)arguments[0] : null, count > 1 ? (T1)arguments[1] : default(T1), count > 2 ? (T2)arguments[2] : default(T2), count > 3 ? (T3)arguments[3] : default(T3)); }
        public Func<Environment, T1, T2, T3, TResult> Function { get; private set; }
    }

    public class Builtin<T1, T2, T3, T4, TResult> : Builtin
    {
        public Builtin(string id, Func<Environment, T1, T2, T3, T4, TResult> function) : base(id, 4) { Delegate = (Function = function); }
        public override object Invoke(params object[] arguments) { var count = Count(arguments); return Function(count > 0 ? (Environment)arguments[0] : null, count > 1 ? (T1)arguments[1] : default(T1), count > 2 ? (T2)arguments[2] : default(T2), count > 3 ? (T3)arguments[3] : default(T3), count > 4 ? (T4)arguments[4] : default(T4)); }
        public Func<Environment, T1, T2, T3, T4, TResult> Function { get; private set; }
    }

    public class Builtin<T1, T2, T3, T4, T5, TResult> : Builtin
    {
        public Builtin(string id, Func<Environment, T1, T2, T3, T4, T5, TResult> function) : base(id, 4) { Delegate = (Function = function); }
        public override object Invoke(params object[] arguments) { var count = Count(arguments); return Function(count > 0 ? (Environment)arguments[0] : null, count > 1 ? (T1)arguments[1] : default(T1), count > 2 ? (T2)arguments[2] : default(T2), count > 3 ? (T3)arguments[3] : default(T3), count > 4 ? (T4)arguments[4] : default(T4), count > 5 ? (T5)arguments[5] : default(T5)); }
        public Func<Environment, T1, T2, T3, T4, T5, TResult> Function { get; private set; }
    }

    public class Lambda : Callable
    {
        public Lambda(object definition, Func<object[], object> closure, int length) : base(length) { Definition = definition; Delegate = (Closure = closure); }
        public override string ToString() { var pair = (object[])Definition; return string.Concat(Syntax.ToString(pair[0]), " => ", Syntax.ToString(pair[1])); }
        public override object Invoke(params object[] arguments) { return Closure(arguments); }
        public Func<object[], object> Closure { get; private set; }
        public object Definition { get; private set; }
    }

    public class Environment : Dictionary<string, object>
    {
        private object[] GetBindings() { var bindings = this.Where(pair => !Equals(pair.Key, string.Empty)).Select(pair => new[] { pair.Key, pair.Value }); var keyed = new HashSet<object>(bindings.Select(pair => pair[0])); var bound = (Outer != null ? Outer.Bindings.Where(pair => !keyed.Contains(((object[])pair)[0])).Concat(bindings) : bindings).ToArray(); return bound; }
        private bool TryGet(string id, ref object value) { if (id != null) { if (!ContainsKey(id)) { return Outer != null ? Outer.TryGet(id, ref value) : false; } else { value = id.Length > 0 ? this[id] : Bindings; return true; } } else return false; }
        private object[] Bindings { get { return GetBindings(); } }
        public Environment(Language evaluator, Environment outer) : this(evaluator, null as IDictionary<string, object>) { Outer = outer; }
        public Environment(Language evaluator, IDictionary<string, object> builtins) { Language = evaluator; if (builtins != null) { builtins.Aggregate(this, (scope, pair) => { scope.Add(pair.Key, pair.Value); return scope; }); } }
        public object Invoke(string id, params object[] arguments) { return Invoke(Get(id), arguments); }
        public object Invoke(object target, params object[] arguments) { var callable = Language.Require(target as Callable); var argv = new object[arguments != null ? arguments.Length + 1 : 1]; var argc = argv.Length; if (argc > 1) { Array.Copy(arguments, 0, argv, 1, argc - 1); } argv[0] = this; return callable.Invoke(argv); }
        public object Get(string id) { object value = null; if (!TryGet(id, ref value)) { throw Language.Error("undefined {0}", Syntax.ToString(id)); } return value; }
        public bool Knows(Symbol symbol) { object value = null; return TryGet(Language.Require(symbol).Id, ref value); }
        public Language Language { get; private set; }
        public Environment Outer { get; private set; }
        public IDictionary<string, object> Bound { get { return Bindings.Cast<object[]>().ToDictionary(binding => (string)binding[0], binding => binding[1]); } }
    }

    public delegate object Evaluator(Environment environment, object expression);

    public class Language
    {
        private Keyword eval;
        private Keyword apply;
        private Keyword quote;
        private Keyword cons;
        private Keyword let;
        private Keyword lambda;
        private Keyword test;

        private Type[] GetSignature(MethodInfo method)
        {
            var parameters = method.GetParameters();
            var length = parameters.Length;
            return parameters.Select(parameter => parameter.ParameterType).Concat(new[] { method.ReturnType }).ToArray();
        }

        private Type GetDelegateType(Type[] signature)
        {
            var delegateTypes = new[] { null, typeof(Func<,>), typeof(Func<,,>), typeof(Func<,,,>), typeof(Func<,,,,>), typeof(Func<,,,,,>), typeof(Func<,,,,,,>) };
            return delegateTypes[signature.Length - 1].MakeGenericType(signature);
        }

        private Type GetBuiltinType(Type[] signature)
        {
            var builtinTypes = new[] { null, typeof(Builtin<>), typeof(Builtin<,>), typeof(Builtin<,,>), typeof(Builtin<,,,>), typeof(Builtin<,,,,>), typeof(Builtin<,,,,,>) };
            return builtinTypes[signature.Length - 1].MakeGenericType(signature.Skip(1).ToArray());
        }

        private IDictionary<string, object> GetReflectedSemantics()
        {
            return
                GetType().
                GetMembers().
                Aggregate
                (
                    new Dictionary<string, object>(),
                    (reflected, member) =>
                    {
                        var attribute = member.GetCustomAttribute<SemanticAttribute>(false);
                        string id;
                        if ((attribute != null) && ((id = attribute.Id) != null))
                        {
                            var property = member as PropertyInfo;
                            var method = member as MethodInfo;
                            var field = member as FieldInfo;
                            object semantic;
                            if (property != null)
                            {
                                if ((property.GetSetMethod() == null) || !property.GetSetMethod().IsPublic)
                                {
                                    semantic = property.GetValue(this);
                                }
                                else
                                {
                                    throw Error("semantic definition property {0} must be read-only", property.Name);
                                }
                            }
                            else if (method != null)
                            {
                                if (attribute is BuiltinAttribute)
                                {
                                    var signature = GetSignature(method);
                                    var delegateType = GetDelegateType(signature);
                                    var builtinType = GetBuiltinType(signature);
                                    var @delegate = Delegate.CreateDelegate(delegateType, this, method);
                                    semantic = Activator.CreateInstance(builtinType, id, @delegate);
                                }
                                else if (attribute is KeywordAttribute)
                                {
                                    var isInfix = ((KeywordAttribute)attribute).IsInfix;
                                    semantic = Activator.CreateInstance(typeof(Keyword), id, Delegate.CreateDelegate(typeof(Semantic), this, method), isInfix);
                                }
                                else
                                {
                                    throw Error("unsupported semantic definition attribute {0}", attribute.GetType());
                                }
                            }
                            else
                            {
                                if (field.IsInitOnly)
                                {
                                    semantic = field.GetValue(this);
                                }
                                else
                                {
                                    throw Error("semantic definition field {0} must be read-only", field.Name);
                                }
                            }
                            AddSemantic(reflected, id, semantic);
                        }
                        return reflected;
                    }
                );
        }

        internal void AddSemantic(IDictionary<string, object> semantics, string id, object semantic) => semantics.Add(id, semantic);

        protected Keyword KeywordOf(Semantic semantic)
        {
            var defined = Require(semantic);
            var keyword = Semantics.Values.FirstOrDefault(builtin => (builtin is Keyword) && Equals(((Keyword)builtin).Semantic, defined)) as Keyword;
            if (keyword == null)
            {
                throw Error("missing reserved keyword definition: {0}", defined.Method.Name);
            }
            return keyword;
        }

        protected virtual Language Validate()
        {
            Reserved.Where(semantic => semantic != Evaluation).Select(semantic => KeywordOf(semantic)).Count();
            return this;
        }

        protected virtual object Evaluation(Environment environment, object expression, bool topLevel)
        {
            var list = expression as object[];
            var symbol = Syntax.Symbol(expression);
            if (environment == null)
            {
                return Validate().Evaluation(new Environment(this, Semantics), expression);
            }
            else if (list != null)
            {
                Keyword keyword;
                if ((list.Length > 1) && ((symbol = Syntax.Symbol(list[1])) != null) && ((keyword = Keyword(symbol.Id)) != null))
                {
                    if (keyword.IsInfix)
                    {
                        var head = Syntax.Symbol(list[0]);
                        var lead = head != null ? Keyword(head.Id) : null;
                        if ((lead == null) || !IsReserved(lead))
                        {
                            return keyword.Semantic(environment, new[] { list[0] }.Concat(list.Skip(2)).ToArray());
                        }
                        throw Error("infix keyword {0} not allowed after reserved keyword {1}", keyword, lead);
                    }
                    throw Error("not an infix keyword at position 2: {0}", keyword);
                }
                if ((list.Length > 0) && ((symbol = Syntax.Symbol(list[0])) != null) && ((keyword = Keyword(symbol.Id)) != null))
                {
                    return keyword.Semantic(environment, list.Skip(1).ToArray());
                }
                else
                {
                    int length;
                    list = list.Select(item => Evaluation(environment, item)).ToArray();
                    if (((length = list.Length) > 0) && ((symbol = Syntax.Symbol(list[0])) != null) && (symbol is Callable))
                    {
                        var callable = (Callable)symbol;
                        var variadic = callable.Variadic;
                        var arguments = new object[variadic ? length - 1 : length];
                        if (length > 1)
                        {
                            Array.Copy(list, 1, arguments, variadic ? 0 : 1, length - 1);
                        }
                        if (variadic)
                        {
                            return callable.Invoke(environment, arguments);
                        }
                        else
                        {
                            arguments[0] = environment;
                            return callable.Invoke(arguments);
                        }
                    }
                    else
                    {
                        return list;
                    }
                }
            }
            else if (symbol != null)
            {
                return (Delegator != null ? Delegator(environment, symbol) : null) ?? environment.Get(symbol.Id);
            }
            else
            {
                return (Delegator != null ? Delegator(environment, expression) : null) ?? expression;
            }
        }

        protected bool IsReserved(Keyword keyword) => Reserved.Contains(keyword.Semantic);
        protected IDictionary<string, object> Semantics { get; private set; }
        protected HashSet<Semantic> Reserved { get; set; }
        protected Keyword Eval { get { return eval = eval ?? KeywordOf(Evaluate); } }
        protected Keyword Apply { get { return apply = apply ?? KeywordOf(Application); } }
        protected Keyword Quote { get { return quote = quote ?? KeywordOf(Quoting); } }
        protected Keyword Cons { get { return cons = cons ?? KeywordOf(Construction); } }
        protected Keyword Let { get { return let = let ?? KeywordOf(Letting); } }
        protected Keyword Lambda { get { return lambda = lambda ?? KeywordOf(Abstraction); } }
        protected Keyword Test { get { return test = test ?? KeywordOf(Testing); } }

        public static T Require<T>(T value)
            where T : class
        {
            if (value != null)
            {
                return value;
            }
            throw Error("value must be a {0}", typeof(T));
        }

        public static KeyValuePair<string, object> Keyword(string id, Semantic semantic) => Keyword(id, semantic, false);
        public static KeyValuePair<string, object> Keyword(string id, Semantic semantic, bool isInfix) => new KeyValuePair<string, object>(id, new Keyword(id, semantic, isInfix));
        public static KeyValuePair<string, object> Builtin(string id, object value) => new KeyValuePair<string, object>(id, value);
        public static KeyValuePair<string, object> Builtin<TResult>(string id, Func<Environment, TResult> function) => new KeyValuePair<string, object>(id, new Builtin<TResult>(id, function));
        public static KeyValuePair<string, object> Builtin<T1, TResult>(string id, Func<Environment, T1, TResult> function) => new KeyValuePair<string, object>(id, new Builtin<T1, TResult>(id, function));
        public static KeyValuePair<string, object> Builtin<T1, T2, TResult>(string id, Func<Environment, T1, T2, TResult> function) => new KeyValuePair<string, object>(id, new Builtin<T1, T2, TResult>(id, function));
        public static KeyValuePair<string, object> Builtin<T1, T2, T3, TResult>(string id, Func<Environment, T1, T2, T3, TResult> function) => new KeyValuePair<string, object>(id, new Builtin<T1, T2, T3, TResult>(id, function));
        public static KeyValuePair<string, object> Builtin<T1, T2, T3, T4, TResult>(string id, Func<Environment, T1, T2, T3, T4, TResult> function) => new KeyValuePair<string, object>(id, new Builtin<T1, T2, T3, T4, TResult>(id, function));
        public static KeyValuePair<string, object> Builtin<T1, T2, T3, T4, T5, TResult>(string id, Func<Environment, T1, T2, T3, T4, T5, TResult> function) => new KeyValuePair<string, object>(id, new Builtin<T1, T2, T3, T4, T5, TResult>(id, function));
        public static Exception Error(string message, params object[] arguments) => new Exception(string.Format(message, arguments));

        public Language()
            : this(null)
        {
        }

        public Language(Syntax syntax)
        {
            Syntax = syntax ?? new Syntax();
            Semantics = new Dictionary<string, object>(GetReflectedSemantics());
            Reserved = new HashSet<Semantic>(new Semantic[] { Evaluation, Application, Quoting, Construction, Letting, Abstraction, Testing });
        }

        public Keyword Keyword(string token) => Semantics.ContainsKey(token) ? Semantics[token] as Keyword : null;
        public Language Include(params KeyValuePair<string, object>[] semanticDefinitions) => semanticDefinitions.Aggregate(this, (language, semanticDefinition) => { language.AddSemantic(Semantics, semanticDefinition.Key, semanticDefinition.Value); return language; });

        public object Evaluate(string input) => Evaluate(null as Environment, new object[] { input });
        public object Evaluate(string input, params object[] arguments) => Evaluate((arguments != null) && (arguments.Length > 0) ? string.Format(input, arguments) : input);
        public object Evaluate(Environment environment, object expression) => Evaluation(environment, Syntax.Parse((string)(expression as object[])[0]), true);

        public object Evaluation(Environment environment, object expression) => Evaluation(environment, expression, false);

        public object Application(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            if (list.Length == 2)
            {
                return Evaluation(environment, new[] { list[0] }.Concat(Evaluation(environment, list[1]) as object[]).ToArray());
            }
            throw Error("list must be a pair");
        }

        public object Quoting(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            if (list.Length == 1)
            {
                return list[0];
            }
            throw Error("list must be a singleton");
        }

        public object Construction(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            if (list.Length == 2)
            {
                return new[] { Evaluation(environment, list[0]) }.Concat(Require(Evaluation(environment, list[1]) as object[])).ToArray();
            }
            throw Error("list must be a pair");
        }

        public object Letting(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            if (list.Length == 2)
            {
                var inner = new Environment(this, environment);
                var assignments = Require(list[0] as object[]);
                for (var i = 0; i < assignments.Length; i++)
                {
                    var assignment = Require(assignments[i] as object[]);
                    if (assignment.Length == 2)
                    {
                        var symbol = Require(assignment[0] as Symbol);
                        var maybeReserved = Keyword(symbol.Id);
                        if ((maybeReserved != null) && IsReserved(maybeReserved))
                        {
                            throw Error("cannot overload reserved keyword: {0}", maybeReserved);
                        }
                        inner[symbol.Id] = Evaluation(environment, assignment[1]);
                    }
                    else
                    {
                        throw Error("list must be a pair");
                    }
                }
                return Evaluation(inner, list[1]);
            }
            throw Error("list must be a pair");
        }

        public object Abstraction(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            var @params = Syntax.Params;
            var @this = Syntax.This;
            if (list.Length == 2)
            {
                var definition = (object[])list.Clone();
                var arguments = Require(list[0] as object[]);
                var length = arguments.Length;
                var formals = new Symbol[length];
                for (var i = 0; i < length; i++)
                {
                    formals[i] = Require(arguments[i] as Symbol);
                }
                var lambda = null as Lambda;
                lambda =
                    new Lambda
                    (
                        definition,
                        delegate (object[] args)
                        {
                            var inner = new Environment(this, environment);
                            var count = Require(args).Length;
                            var i = 0;
                            if (count > 0)
                            {
                                while (i < length)
                                {
                                    var formal = formals[i++];
                                    if (i < count)
                                    {
                                        inner[formal.Id] = args[i];
                                    }
                                }
                                if (@params != null)
                                {
                                    inner[@params.Id] = args.Skip(1).ToArray();
                                }
                                if (@this != null)
                                {
                                    inner[@this.Id] = lambda;
                                }
                                inner[lambda.Id] = lambda;
                                return Evaluation(inner, definition[1]);
                            }
                            throw Error("invalid invocation");
                        },
                        length
                    );
                return lambda;
            }
            throw Error("list must be a pair");
        }

        public object Testing(Environment environment, object expression)
        {
            var list = Require(expression as object[]);
            if (list.Length == 3)
            {
                return (bool)Evaluation(environment, list[0]) ? Evaluation(environment, list[1]) : Evaluation(environment, list[2]);
            }
            else if (list.Length == 2)
            {
                return Evaluation(environment, new[] { Test, new[] { Test, list[0] }, list[0], list[1] });
            }
            else if (list.Length == 1)
            {
                return environment.Knows(Require(list[0] as Symbol));
            }
            else
            {
                throw Error("list must be a singleton, a pair, or a triple");
            }
        }

        public Evaluator Delegator { get; protected set; }
        public Syntax Syntax { get; private set; }
    }
}
/*
System.Language.TypeInference ( https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system )

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

namespace System.Language.TypeInference
{
    #region Abstract syntax tree
    public abstract class Node
    {
        public static Const Const(object value) => new Const { Spec = value != null ? (!(value is IType) ? value.GetType().FullName : value) : typeof(void).FullName };
        public static Var Var(string name) => Var(name, null);
        public static Var Var(string name, object type) => new Var { Spec = name, Type = type };
        public static Apply Apply(Node expr, Node[] args) => Apply(expr, args, null);
        public static Apply Apply(Node expr, Node[] args, bool isAnnotation) => Apply(expr, args, null, isAnnotation);
        public static Apply Apply(Node expr, Node[] args, object ctor) => Apply(expr, args, ctor, false);
        public static Apply Apply(Node expr, Node[] args, object ctor, bool isAnnotation) => new Apply { Spec = expr, Args = args, Type = ctor, IsAnnotation = isAnnotation };
        public static Abstract Abstract(Node[] args, Node body) => Abstract(args, null, body);
        public static Abstract Abstract(Node[] args, object type, Node body) => new Abstract { Args = args, Body = body, Type = type };
        public static Define Define(Var var, Node body) => new Define { Spec = var, Body = (body as Node) ?? Const(body) };
        public static Let Let(Define[] defs, Node body) => new Let { Args = defs, Body = body };
        public abstract IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types);
        public object Spec { get; set; }
        public Node[] Args { get; set; }
        public Node Body { get; set; }
        public object Type { get; set; }
        public bool IsAnnotation { get; private set; }
    }

    public class Const : Node
    {
        public override string ToString() => string.Concat("{", Spec, "}");
        public override IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types) => !(Spec is IType) ? system.Const(env, (string)Spec) : (IType)Spec;
    }

    public class Var : Node
    {
        public override string ToString() => Id;
        public override IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types) => system.Fresh(env[Id], types.ToArray());
        public string Id => (string)Spec;
    }

    public class Abstract : Node
    {
        public override string ToString() => string.Format("( {0}){2} => {1}", Args.Length > 0 ? string.Concat(string.Join(" ", Args.Select(arg => arg.ToString()).ToArray()), " ") : string.Empty, Body, Type != null ? string.Concat(" : ", Type) : string.Empty);
        public override IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types)
        {
            var scope = system.NewEnvironment(env);
            var known = new List<IType>(types);
            var args = new List<IType>();
            foreach (var arg in Args)
            {
                IType type;
                Var var;
                if (!(arg is Define))
                {
                    var = (Var)arg;
                    if (var.Type != null)
                    {
                        type = !(var.Type is IType) ? system.Const(scope, (string)var.Type) : (IType)var.Type;
                    }
                    else
                    {
                        type = system.NewGeneric();
                        known.Add(type);
                    }
                    scope[var.Id] = type;
                }
                else
                {
                    var spec = ((Define)arg).Body;
                    var = (Var)((Define)arg).Spec;
                    type = system.NewGeneric();
                    system.Unify(type, system.Infer(scope, spec, known));
                    scope[var.Id] = type;
                    known.Add(type);
                }
                args.Add(type);
            }
            args.Add(system.Infer(scope, Body is Let ? Body.Args[Body.Args.Length - 1] : Body, known));
            if (Type != null)
            {
                system.Unify(args[args.Count - 1], !(Type is IType) ? system.Const(scope, (string)Type) : (IType)Type);
            }
            return system.NewType(TypeSystem.Function, args.ToArray());
        }
    }

    public class Apply : Node
    {
        private bool IsFunction(IType type) => type != null ? (type.Constructor != null ? type.Constructor == TypeSystem.Function : IsFunction(type.Self)) : false;
        private Node ToFormal(ITypeSystem system, IEnvironment env, IList<IType> types, Node arg)
        {
            Func<Var, IType> typed =
                var =>
                    IsAnnotation && !env.ContainsKey(var.Id) ? env[var.Id] = system.NewGeneric() : env[var.Id];
            var formal =
                IsAnnotation ?
                (
                    arg is Apply ?
                    Define((Var)arg.Args[0], Const(system.Infer(env, (Node)arg.Spec, types)))
                    :
                    arg is Var ? Define((Var)arg, Const(typed((Var)arg))) : arg
                )
                :
                arg is Var ? Define((Var)arg, Const(typed((Var)arg))) : arg;
            return formal;
        }
        private IType AsAnnotationType(ITypeSystem system, IEnvironment env, IList<IType> types)
        {
            IType ctor;
            if
            (
                (Spec is Node) &&
                (((ctor = ((Node)Spec).Type as IType)) != null) &&
                (!IsFunction(ctor) || IsAnnotation)
            )
            {
                Args.Select
                    (
                        (arg, i) =>
                            arg is Var ?
                            system.Infer(env, Define((Var)arg, Const(ctor[i])), types)
                            :
                            null
                    ).
                    ToArray();
                return system.NewType(ctor, Args.Select(arg => system.Infer(env, ToFormal(system, env, types, arg), types)).ToArray());
            }
            else
            {
                return null;
            }
        }
        public override string ToString() => string.Format("( {0} {1})", Spec, Args.Length > 0 ? string.Concat(string.Join(" ", Args.Select(arg => arg.ToString()).ToArray()), " ") : string.Empty);
        public override IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types)
        {
            if (Type == null)
            {
                var annotation = AsAnnotationType(system, env, types);
                if (annotation != null)
                {
                    return annotation;
                }
            }
            var args = Args.Select(arg => system.Infer(env, ToFormal(system, env, types, arg), types)).ToList();
            var expr = (Node)Spec;
            var self = system.Infer(env, expr, types);
            var @out = system.NewGeneric();
            if (Type != null)
            {
                var ctor = !(Type is IType) ? system.Const(env, (string)Type) : (IType)Type;
                @out = system.Infer(env, Apply(Var(ctor.Id, ctor), args.Select(arg => Const(arg)).ToArray(), IsAnnotation), types);
            }
            else
            {
                args.Add(@out);
                system.Unify(system.NewType(TypeSystem.Function, args.ToArray()), self);
            }
            return @out;
        }
    }

    public class Define : Node
    {
        public override string ToString() => string.Format("( {0} = {1} )", Spec, Body);
        public override IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types)
        {
            var known = new List<IType>(types);
            var type = system.NewGeneric();
            var var = (Var)Spec;
            env[var.Id] = type;
            known.Add(type);
            system.Unify(type, system.Infer(env, Body, known));
            return type;
        }
    }

    public class Let : Node
    {
        public override string ToString() => string.Format("( ( {0}) {1} )", Args.Length > 0 ? string.Concat(string.Join(" ", Args.Select(arg => arg.ToString()).ToArray()), " ") : string.Empty, Body);
        public override IType Infer(ITypeSystem system, IEnvironment env, IList<IType> types)
        {
            env = system.NewEnvironment(env);
            return Args.Select(let => let.Infer(system, env, types)).Concat(new[] { Body.Infer(system, env, types) }).Last();
        }
    }
    #endregion

    #region Type schemes
    public interface IType
    {
        IType Constructor { get; }
        string Id { get; }
        IType[] Args { get; }
        IType Self { get; }
        IType this[int index] { get; }
        object Metadata { get; }
    }
    #endregion

    #region Environment
    public interface IEnvironment : IDictionary<string, IType>
    {
    }

    public class Environment : Dictionary<string, IType>, IEnvironment
    {
        protected virtual IType Get(string id)
        {
            if (id == null)
            {
                throw new ArgumentNullException("id", "cannot be null");
            }
            if (!ContainsKey(id))
            {
                throw new InvalidOperationException(string.Format("undefined {0}", id));
            }
            return base[id];
        }
        protected virtual void Set(string id, IType type)
        {
            if (id == null)
            {
                throw new ArgumentNullException("id", "cannot be null");
            }
            base[id] = type;
        }

        public Environment() : this(null) { }
        public Environment(IDictionary<string, IType> outer) : base(outer ?? new Dictionary<string, IType>()) { }
        public new IType this[string id] { get { return Get(id); } set { Set(id, value); } }
    }
    #endregion

    #region Type system
    public interface ITypeSystem
    {
        IEnvironment NewEnvironment();
        IEnvironment NewEnvironment(IDictionary<string, IType> outer);
        IType Fresh(IType t, IType[] types);
        IType Const(IEnvironment env, string ctor);
        IType NewGeneric();
        IType NewType(string id, IType[] args);
        IType NewType(string id, IType[] args, object meta);
        IType NewType(IType constructor, IType[] args);
        IType NewType(IType constructor, IType[] args, object meta);
        IType NewType(IType constructor, string id, IType[] args);
        IType NewType(IType constructor, string id, IType[] args, object meta);
        void Unify(IType t, IType s);
        IType Infer(IEnvironment env, Node node);
        IType Infer(IEnvironment env, Node node, IList<IType> types);
    }

    public class TypeSystem : ITypeSystem
    {
        internal abstract class Scheme : IType
        {
            protected Scheme(string id) : this(id, null) { }
            protected Scheme(string id, IType[] args) { if ((this is Type) && (id == null)) { throw new ArgumentNullException("id", "cannot be null"); } Id = id; Args = args ?? new IType[0]; }
            public override string ToString() => Id;
            public IType Constructor { get; protected set; }
            public virtual string Id { get; protected set; }
            public IType[] Args { get; private set; }
            public IType Self { get; internal set; }
            public object Metadata { get; protected set; }
            public IType this[int index] => Args[index];
        }

        internal class Generic : Scheme
        {
            private string alpha;
            private string Alpha() { var id = Uid; var sb = new StringBuilder(); while (id-- > 0) { var r = id % 26; var c = (char)(r + 97); sb.Insert(0, c); id = (id - r) / 26; } return sb.ToString(); }
            private string GetName() => Self != null ? Self.Id : string.Concat('`', alpha ?? (alpha = Alpha()));
            internal Generic(TypeSystem system) : base(null) { Uid = system.NewId(); }
            internal readonly int Uid;
            public override string ToString() => Self != null ? Self.ToString() : base.ToString();
            public override string Id { get { return GetName(); } protected set { } }
        }

        internal class Type : Scheme
        {
            internal Type(IType constructor, string id, IType[] args, object meta) : base(id, args) { Constructor = constructor ?? this; Metadata = meta; }
            public override string ToString() { var id = Args.Length > 0 ? Id : base.ToString(); return (Args.Length > 0 ? string.Format("{0}<{1}>", id, string.Concat(string.Join(", ", Args.Take(Args.Length - 1).Select(arg => arg.ToString())), (Args.Length > 1 ? ", " : string.Empty), Args[Args.Length - 1].ToString())) : id); }
        }

        private static IType Create(IType constructor, string id, IType[] args, object meta) => new Type(constructor, id, args, meta);
        private static IType Prune(IType t)
        {
            Generic var = t as Generic;
            return (var != null) && (var.Self != null) ? (var.Self = Prune(var.Self)) : t;
        }
        private static bool OccursIn(IType t, IType s) => ((s = Prune(s)) != t) ? (s is Type ? OccursIn(t, s.Args) : false) : true;
        private static bool OccursIn(IType t, IType[] types) => types.Any(type => OccursIn(t, type));
        private IType Fresh(IType t, IType[] types, IDictionary<int, IType> vars)
        {
            vars = vars ?? new Dictionary<int, IType>();
            t = Prune(t);
            var var = t as Generic;
            var type = t as Type;
            if (var != null)
            {
                if (!OccursIn(t, types))
                {
                    if (!vars.ContainsKey(var.Uid))
                    {
                        vars[var.Uid] = NewGeneric();
                    }
                    return vars[var.Uid];
                }
                else
                {
                    return t;
                }
            }
            else if (type != null)
            {
                return NewType(type.Constructor, type.Id, type.Args.Select(arg => Fresh(arg, types, vars)).ToArray());
            }
            else
            {
                throw new InvalidOperationException(string.Format("unsupported {0}", t.GetType()));
            }
        }
        private int id;
        internal int NewId() => ++id;

        public static readonly IType Function = Create(null, "Func", null, null);

        #region ITypeSystem
        public virtual IEnvironment NewEnvironment() => NewEnvironment(null);
        public virtual IEnvironment NewEnvironment(IDictionary<string, IType> outer) => new Environment(outer);
        public IType Fresh(IType t, IType[] types) => Fresh(t, types, null);
        public IType Const(IEnvironment env, string ctor) => env[ctor];
        public IType NewGeneric() => new Generic(this);
        public IType NewType(string id, IType[] args) => NewType(id, args, null);
        public IType NewType(string id, IType[] args, object meta) => NewType(null, id, args, meta);
        public IType NewType(IType constructor, IType[] args) => NewType(constructor, args, null);
        public IType NewType(IType constructor, IType[] args, object meta) => NewType(constructor, constructor.Id, args, meta);
        public IType NewType(IType constructor, string id, IType[] args) => NewType(constructor, id, args, null);
        public IType NewType(IType constructor, string id, IType[] args, object meta) => Create(constructor, id, args, meta);
        public void Unify(IType t, IType s)
        {
            t = Prune(t);
            s = Prune(s);
            if (t is Generic)
            {
                if (t != s)
                {
                    if (OccursIn(t, s))
                    {
                        throw new InvalidOperationException(string.Format("recursive unification of {0} in {1}", t, s));
                    }
                    ((Generic)t).Self = s;
                }
            }
            else if ((t is Type) && (s is Generic))
            {
                Unify(s, t);
            }
            else if ((t is Type) && (s is Type))
            {
                var t_type = (Type)t;
                var s_type = (Type)s;
                if ((t_type.Constructor.Id != s_type.Constructor.Id) || (t_type.Args.Length != s_type.Args.Length))
                {
                    throw new InvalidOperationException(string.Format("{0} incompatible with {1}", t_type, s_type));
                }
                for (var i = 0; i < t_type.Args.Length; i++)
                {
                    Unify(t_type.Args[i], s_type.Args[i]);
                }
            }
            else
            {
                throw new InvalidOperationException(string.Format("undecided unification for {0} and {1}", t, s));
            }
        }
        public IType Infer(IEnvironment env, Node node) => Infer(env, node, null);
        public IType Infer(IEnvironment env, Node node, IList<IType> types) => node.Infer(this, env, types ?? new List<IType>());
        #endregion
    }
    #endregion
}

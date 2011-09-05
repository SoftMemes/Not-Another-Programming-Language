using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SoftMemes.Napl.Runtime
{
    public static class RuntimeFunctions
    {
        public static Func<T1, TRet> Curry<T1, TRet>(Func<Tuple<T1>, TRet> f)
        {
            return new Func<T1,TRet>(x1 => f(Tuple.Create(x1)));
        }

        public static Func<T1, T2, TRet> Curry<T1, T2, TRet>(Func<Tuple<T1, T2>, TRet> f)
        {
            return new Func<T1, T2, TRet>((x1, x2) => f(Tuple.Create(x1, x2)));
        }

        public static Func<T1, T2, T3, TRet> Curry<T1, T2, T3, TRet>(Func<Tuple<T1, T2, T3>, TRet> f)
        {
            return new Func<T1, T2, T3, TRet>((x1, x2, x3) => f(Tuple.Create(x1, x2, x3)));
        }

        public static Func<Tuple<T1>, TRet> Uncurry<T1, TRet>(Func<T1, TRet> f)
        {
            return new Func<Tuple<T1>, TRet>(x => f(x.Item1));
        }

        public static Func<Tuple<T1, T2>, TRet> Uncurry<T1, T2, TRet>(Func<T1, T2, TRet> f)
        {
            return new Func<Tuple<T1, T2>, TRet>(x => f(x.Item1, x.Item2));
        }

        public static Func<Tuple<T1, T2, T3>, TRet> Uncurry<T1, T2, T3, TRet>(Func<T1, T2, T3, TRet> f)
        {
            return new Func<Tuple<T1, T2, T3>, TRet>(x => f(x.Item1, x.Item2, x.Item3));
        }
    }
}

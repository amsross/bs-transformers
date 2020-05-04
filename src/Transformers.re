module Infix = BsBastet.Infix;
module Interface = BsBastet.Interface;

module type Def = {
  type m('a);
  type t('a);

  include Interface.MONAD with type t('a) := t('a);
};

module type T = {
  include Def;

  let lift: m('a) => t('a);

  module Infix: {
    let (<$>): ('a => 'b, t('a)) => t('b);
    let (<#>): (t('a), 'a => 'b) => t('b);
    let (<*>): (t('a => 'b), t('a)) => t('b);
    let (>>=): (t('a), 'a => t('b)) => t('b);
    let (=<<): ('a => t('b), t('a)) => t('b);
    let (>=>): ('a => t('b), 'b => t('c), 'a) => t('c);
    let (<=<): ('a => t('b), 'c => t('a), 'c) => t('b);
  };
};

module StateT = (T: Interface.TYPE, M: Interface.MONAD) => {
  type m('a) = M.t('a);
  type t('a) = T.t => m((T.t, 'a));

  module D: Interface.MONAD with type t('a) = t('a) = {
    type nonrec t('a) = t('a);

    let pure: 'a. 'a => t('a) = (a, s) => M.pure((s, a));

    let flat_map: 'a 'b. (t('a), 'a => t('b)) => t('b) =
      (old, aToState, s) => {
        let m = old(s);

        M.flat_map(m, ((s, a)) => aToState(a, s));
      };

    let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
      (f, old, s) => old(s) |> M.map(((s, a)) => (s, f(a)));

    let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
      (mf, m) => flat_map(mf, f => map(f, m));
  };

  module Infix = Infix.Monad(D);
  let lift: 'a. M.t('a) => D.t('a) = (a, s) => M.map(a => (s, a), a);

  include (D: Interface.MONAD with type t('a) := t('a));

  let get: t(T.t) = s => M.pure((s, s));
  let put: T.t => t(unit) = (s, _) => M.pure((s, ()));
  let modify: (T.t => T.t) => t(T.t) =
    (f, s) => {
      let s' = f(s);
      M.pure((s', s'));
    };
};

module ReaderT = (T: Interface.TYPE, M: Interface.MONAD) => {
  type m('a) = M.t('a);
  type t('a) = T.t => m('a);

  module D: Interface.MONAD with type t('a) = t('a) = {
    type nonrec t('a) = t('a);

    let pure: 'a. 'a => t('a) = (a, _) => M.pure(a);

    let flat_map: 'a 'b. (t('a), 'a => t('b)) => t('b) =
      (old, aToState, s) => {
        let m = old(s);

        M.flat_map(m, a => aToState(a, s));
      };

    let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
      (f, old, s) => old(s) |> M.map(a => f(a));

    let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
      (mf, m) => flat_map(mf, f => map(f, m));
  };

  module Infix = Infix.Monad(D);
  let lift: 'a. M.t('a) => D.t('a) = (a, _) => M.map(a => a, a);

  include (D: Interface.MONAD with type t('a) := t('a));

  let ask: unit => t(T.t) = (_, s) => M.pure(s);
};

module OptionT:
  (M: Interface.MONAD) =>
   T with type m('a) = M.t('a) and type t('a) = M.t(option('a)) =
  (M: Interface.MONAD) => {
    type m('a) = M.t('a);
    type t('a) = m(option('a));

    module D: Interface.MONAD with type t('a) = t('a) = {
      type nonrec t('a) = t('a);

      let pure: 'a. 'a => t('a) = x => M.pure(Some(x));

      let flat_map: 'a 'b. (t('a), 'a => t('b)) => t('b) =
        (m, f) =>
          M.flat_map(
            m,
            fun
            | None => M.pure(None)
            | Some(x) => f(x),
          );

      let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
        (f, m) => flat_map(m, m => M.pure(Some(f(m))));

      let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
        (mf, m) => flat_map(mf, f => map(f, m));
    };

    module Infix = Infix.Monad(D);
    let lift: 'a 'b. M.t('a) => D.t('a) = x => M.flat_map(x, D.pure);

    include (D: Interface.MONAD with type t('a) := t('a));
  };

module ResultT:
  (T: Interface.TYPE, M: Interface.MONAD) =>
   T with type m('a) = M.t('a) and type t('a) = M.t(result('a, T.t)) =
  (T: Interface.TYPE, M: Interface.MONAD) => {
    type m('a) = M.t('a);
    type t('a) = M.t(result('a, T.t));

    module D: Interface.MONAD with type t('a) = t('a) = {
      type nonrec t('a) = t('a);

      let pure: 'a. 'a => t('a) = x => M.pure(Ok(x));

      let flat_map: 'a 'b. (t('a), 'a => t('b)) => t('b) =
        (m, f) =>
          M.flat_map(
            m,
            fun
            | Error(_) as err => M.pure(err)
            | Ok(x) => f(x),
          );

      let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
        (f, m) => flat_map(m, m => M.pure(Ok(f(m))));

      let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
        (mf, m) => flat_map(mf, f => map(f, m));
    };

    module Infix = Infix.Monad(D);
    let lift: 'a. M.t('a) => D.t('a) = x => M.flat_map(x, D.pure);

    include (D: Interface.MONAD with type t('a) := t('a));
  };

module ContT:
  (T: Interface.TYPE, M: Interface.MONAD) =>

    T with
      type m('a) = M.t('a) and type t('a) = ('a => M.t(T.t)) => M.t(T.t) =
  (T: Interface.TYPE, M: Interface.MONAD) => {
    type m('a) = M.t('a);
    type t('a) = ('a => m(T.t)) => m(T.t);

    module D: Interface.MONAD with type t('a) = t('a) = {
      type t('a) = ('a => m(T.t)) => m(T.t);

      let pure: 'a. 'a => t('a) = (x, k) => k(x);

      let flat_map: 'a 'b. (t('a), 'a => t('b)) => t('b) =
        (m, f, k) => m(v => f(v, k));

      let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
        (f, m) => flat_map(m, (m, k) => k(f(m)));

      let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
        (mf, m) => flat_map(mf, f => map(f, m));
    };

    module Infix = Infix.Monad(D);
    let lift: 'a. M.t('a) => D.t('a) = M.flat_map;

    include (D: Interface.MONAD with type t('a) := t('a));
  };

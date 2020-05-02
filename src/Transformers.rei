module type T = {
  type m('a);
  type t('a);

  include BsBastet.Interface.MONAD with type t('a) := t('a);

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

module OptionT:
  (M: BsBastet.Interface.MONAD) =>
   T with type m('a) = M.t('a) and type t('a) = M.t(option('a));

module ResultT:
  (T: BsBastet.Interface.TYPE, M: BsBastet.Interface.MONAD) =>
   T with type m('a) = M.t('a) and type t('a) = M.t(result('a, T.t));

module ContT:
  (T: BsBastet.Interface.TYPE, M: BsBastet.Interface.MONAD) =>

    T with
      type m('a) = M.t('a) and type t('a) = ('a => M.t(T.t)) => M.t(T.t);

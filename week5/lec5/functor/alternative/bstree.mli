include module type of Bstree_intf

module Make(Ord : OrderedType) : S with type elt = Ord.t

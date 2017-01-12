package pavlosgi.freecli.command

object all extends AllOps with AllImplicits {
  type ParentWith[P, B] = api.ParentWith[P, B]
}


Some notes by @roberth, explaning technical design decisions and formatting
decisions.

We can avoid a full blown pretty printing library, because
those are designed to do fancy 2D layouts, yet they are too syntax
directed. What we typically want for merge-friendly layout is at
most two layouts per given node type. A single-line one and a
multiline one. Expanding all ancestry to multi-line layout greedily
whenever a line is full seems like a good idea:
  - multi-line is good for merges, so we want more of it
  - greedy is good because it reduces the number of possible
    solutions for a given source text, reducing conflicts
  - if someone edits the single line without having had to expand it
    we would have had a merge conflict anyway

So it's important here that a single vs. multiline decision only
affects one place. For example:

    Input:
  
    {
      a = too long for line;
      b = short;
    }

    Desirable:                 Not desirable:
    {                          {
      a =                        a =
        too long                   too long
          for line;                  for line;
      b = short;           .-->  b =
    }                     / .->    short;
      potential extra   ---'   }
         conflict

The right hand side looks nicer, but does introduce an extra conflict
if someone else edited b before the value for a became too long.

Another rule to use is that multiline nodes must be formatted in the
multiline format again. Use of multiline syntax can be a hint that a
piece of code may be likely to change. For example, a short list may
be written in multiline syntax, because the user knows that the list will
be extended.

This can be implementated by the formatting abstraction: it can check
whether all Nodes it contains are on the same line and if not, force
the multiline layout.

We've been working on ByteStrings because that's what both Nix and
tree-sitter do. Sticking to ByteStrings may be nice. We can still
count UTF8 code points to make it nicer. (I know code points aren't
glyphs, but you have to draw a line (no pun intended) - what's the width
of a 'glyph' anyway?)

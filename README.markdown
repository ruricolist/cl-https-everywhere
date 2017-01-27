CL-HTTPS-EVERYWHERE parses [HTTPS Everywhere][] rulesets and makes
them available for use in Lisp programs.

I have not included a copy of the rulesets, simply because I am not
sure what license they are available under. You can fetch and build
the rulesets using the Makefile.

The sole exported function is `rewrite-uri`, which takes a URI as a
string, rewrites it if possible, and returns three values:

- The possibly rewritten URI (a string);
- Whether the URI returned is HTTPS;
- And whether any rewriting was done.

Three values are necessary to distinguish the case where the URI
passed in was *already* an HTTPS URI.

    (rewrite-uri "http://example.com/")
    => "http://example.com/", NIL, NIL

    (rewrite-uri "http://www.eff.org/")
    => "https://www.eff.org/", T, T

    (rewrite-uri "https://www.eff.org/")
    => "https://www.eff.org/", T, NIL

[HTTPS Everywhere]: https://www.eff.org/HTTPS-everywhere

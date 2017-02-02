CL-HTTPS-EVERYWHERE parses [HTTPS Everywhere][] rulesets and makes
them available for use in Lisp programs.

I have not included a copy of the rulesets, simply because I am not
sure what license they are available under. They will be automatically
fetched when first loading the system, and updated on subsequent
loads.

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

At the moment the rulesets are fetched by shallow-cloning the HTTPS
Everywhere repository. It is possible that at some point in the future
there may be [a simpler API][]. If such an API does come along, it
would make keeping the rulesets up-to-date much easier.

[HTTPS Everywhere]: https://www.eff.org/HTTPS-everywhere
[Overlord]: https://github.com/TBRSS/overlord
[API]: https://github.com/EFForg/https-everywhere/issues/6937

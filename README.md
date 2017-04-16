# NetApps
CS7009 work

The page allows you to query the database and view nodes and links for arbitrary queries.

At the moment the page is configured to give best visual results for smaller queries, and the crawler is set (in githubauth/Handler/Crawl.hs) to jump through 15 nodes -- which produces (with my profile as the starting point) something on the order of 1000 users and 3000 relationships. It is best not to try view all of these at once.

The "interesting question" which I attempted to answer was "What groups of languages frequently co-occur?". To that end, I've modified the force directed graph and the database, so that languages link to each other with "WITH" relationships. These relationships have a property, /num/, which defines how often that relationship occurs. The visualisation takes this property into account and increases the width of connecting links as the square root (for practicality's sake) of the number of co-occurrences.

You can see the answer to this question by running a query such as:

MATCH path = (n:Language)-[r:WITH]-(n2:Language)
WHERE r.num>25 RETURN path

Where we filter by 25 co-occurences to eliminate some noise (with ~1000 repos, someone's bound to use odd combinations)

I've included a picture of the results on my end with that query, which show groupings for "web dev" languages like JavaScript and HTML, and groupings for C-style languages (C, C++, makefile). This is what I intuitively expected to occur (and indeed is borne out by the recent stack overflow developer survey results which show similar results from different methods).

To simply get an idea of your local github network, run a query like

MATCH path = (u:User)-[r:CONTRIBS]->(p:Repo)
LIMIT 100
RETURN path

Hint: you might want to dial down the strength of the repulsive node force in profile.julius, for large result sets things tend to fly away.
# Regex stuff

s <- "I cannot accept the assumptions and implications inherent in that policy. We need to remind ourselves of the central facts, which are these: there are millions of EU citizens—maybe over 3 million—who have come to this country in the legitimate expectation that they will be able to live and work here for as long as they choose. For many of them, that has been a career-changing, maybe even a life-changing, decision, which may be irrevocable. Their decision was entirely reasonable and proper, based on their assumptions. It accorded with the law that then existed. It accords with the law that exists today. For the United Kingdom now to disturb that expectation would involve an act of retrospective legislation and policy that would offend natural justice and, I suspect, the principles of human rights legislation."

# basic match
str_view_all(s, "for")
str_view_all(s, "For")

# meta characters
# "." match any character
str_view_all(s, "for...")

# [] match any character contained in the square brackets
str_view_all(s, "[aieou]n")

# [^ ] negate any character contained in the square brackets
str_view_all(s, "[^aeiou]n")

# * + ? match the next 0+, 1+ repititions of the last pattern, or make it optional
str_view_all(s, "[aeiou]+")
str_view_all(s, "m\\w*s")
str_view_all(s, "thes?")

# {n,m} match the next n but not more than m repititions
str_view_all(s, "a\\w{3,4}")

# () group a set of characters
str_view_all(s, "(at|ex)")
str_view_all(s, "(?:at|ex)") # A "non-capturing" group allows you to match characters but
# not capture the group- meaning it isn't retained for later use. 

# ^match the beginning of the string, $ match the end
str_view_all(s, "^I....")
str_view_all(s, "lation.")
str_view_all(s, "lation.$")

# \\w alphanumeric characters \\W non-am characters \\d digits \\s whitespace characters
str_view_all(s, "\\sc\\w+\\s")

# Lookaround

# positive and negative lookahead

str_view_all(s, "(T|t)he(?=\\sassumptions)") # positive- the first part of the expression must 
# be followed by the lookahead
str_view_all(s, "(T|t)he(?!\\sassumptions)") # negative- all patterns that aren't followed by the lookahead

# positive and negative lookbehind

str_view_all(s, "(?<=here\\s)([f|F]or)") # all for words preceded by here
str_view_all(s, "(?<!here\\s)([f|F]or)") # all for words not preceded by here
str_view_all(s, "(?<!t)(h)") # all t not followed by h



# references ----

foo(?!.*foo) # matches the last occurance of foo in the string. 

# regex tutorial notes ----

# intro + characters sets
# regex always matches the left-most match. regex tries all permutations of a pattern from
# a character, and can backtrack. 

# Find alternate characters at a position using character sets
str_view(s, "sep[ae]r[ae]te") # match any separate, separete, seperate, seperete

# These sets can be negated using ^
str_view(s, "[^0-9\r\n]") # match character this is not numeric or a line break

# Negation means: "match a q followed by any character that is not u"
str_view("iraq", "q[^u]") # i.e. there has to be a character following q, here there's no match
str_view("iraq is a country", "q[^u]") # here the match occurs on the q and a space

# If you just wanted to match the q, you would use a negative lookahead
str_view("iraq", "q(?!u)")

# Matching escaped characters can be done using a character set
str_view("ago\\obo", "[\\\\b]") # This does not match, escaped b, it matches backslash or b

# Repeating characters can be matched
str_view("There are 1000 grapes in here!", "[0-9]+")

# And to repeat the matched character you can use a backreference
str_view("There are 1000 grapes in here!", "([0-9])\\1+")





# sources ----

# non-capturing/capturing groups
# https://stackoverflow.com/questions/3512471/what-is-a-non-capturing-group-what-does-do

all: rulesets.xml

.PHONY: clean maintainer-clean

rules = https-everywhere/src/chrome/content/rules

$(rules):
	git submodule update --init --depth 1

rulesets.xml: | $(rules)
	echo "<rulesets>" > rulesets.xml
	cat $(rules)/*.xml >> rulesets.xml
	echo "</rulesets>" >> rulesets.xml

clean:
	rm -rf https-everywhere/*

maintainer-clean: clean
	rm rulesets.xml

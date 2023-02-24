all:
	sbt compile assembly
	chmod +x compile
clean:
	sbt clean

.PHONY: all clean

default:
	mono lib/FAKE.2.4.2.0/tools/FAKE.exe build.fsx $(target)

test:
	mono lib/FAKE.2.4.2.0/tools/FAKE.exe build.fsx BuildTest
	mono lib/NUnit.Runners.2.6.3/tools/nunit-console.exe tests/BitCoinFs.UnitTests.dll -run=$(run)

reload:
	mono lib/FAKE.2.4.2.0/tools/FAKE.exe build.fsx BuildTest
	#rm -rf ~/Downloads/neo4j-community-2.0.0/data/graph.db/
	#~/Downloads/neo4j-community-2.0.0/bin/neo4j restart
	mono build/BitcoinFs.Neo4jClient.exe --limit-to 50

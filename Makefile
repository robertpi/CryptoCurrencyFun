default:
	mono lib/FAKE.2.4.2.0/tools/FAKE.exe build.fsx $(target)

test:
	mono lib/FAKE.2.4.2.0/tools/FAKE.exe build.fsx BuildTest
	mono lib/NUnit.Runners.2.6.3/tools/nunit-console.exe tests/BitCoinFs.UnitTests.dll -run=$(run)
#I "lib/FAKE.2.4.2.0/tools"
#r "FakeLib.dll" // include Fake lib

open Fake

// Directories
let buildDir  = "./build/"
let testDir   = "./tests/"
let deployDir = "./deploy/"

// Filesets
let appReferences  = 
    !! "src/**/*.fsproj"

let testReferences = !! "src/tests/**/*.fsproj"

// The clean target cleans the build and deploy folders
Target "Clean" (fun _ -> 
    CleanDirs ["./build/"; "./deploy/"]
)

Target "BuildApp" (fun _ ->
    // compile all projects below src/app/
    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    MSBuildDebug testDir "Build" testReferences
        |> Log "TestBuild-Output: "
)

// define test dlls
let testDlls = !! (testDir + "/*.UnitTests.dll")

Target "NUnitTest" (fun _ ->
    testDlls
        |> NUnit (fun p -> 
            {p with
                DisableShadowCopy = true; 
                OutputFile = testDir + "TestResults.xml"})
)

// Build order
"Clean"
  ==> "BuildApp"
  ==> "BuildTest"
  ==> "NUnitTest"

// start build
RunTargetOrDefault "NUnitTest"
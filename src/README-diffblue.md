# Generating Tests for Apache Ant

Choosing Apache Ant was fairly arbitrary, but it uses a recent version of JUnit and is still
under development, it should be representative of what we'd see at customer's sites.

## Building Apache Ant

Following the guide on their website:

1. Download and install the `ant` build system from your package manager
2. Download and extract the [ant sources](https://github.com/apache/ant)
3. Run `ant -f fetch.xml -Ddest=optional` in the clone of the repo (this fetches all the
   dependencies it needs)

At this point you'll have an error message:

```text
BUILD FAILED
/path/to/projects/ant/fetch.xml:371: The following error occurred while executing this line:
/path/to/projects/ant/fetch.xml:123: Could not resolve artifacts: Could not find artifact javax.media:jai-core:jar:1.1.3 in central (https://repo1.maven.org/maven2/)
```

There are _two_ dependencies that are missing, once you've downloaded them put them in
`lib/optional`. You can download the `jar` files from the following URLs:

* [https://repository.jboss.org/maven2/javax/media/jai-core/1.1.3/jai-core-1.1.3.jar](https://repository.jboss.org/maven2/javax/media/jai-core/1.1.3/jai-core-1.1.3.jar)
* [http://maven.geotoolkit.org/javax/media/jai_codec/1.1.3/jai_codec-1.1.3.jar](http://maven.geotoolkit.org/javax/media/jai_codec/1.1.3/jai_codec-1.1.3.jar)

Now that you've got the complete list of dependencies, you need to comment out the _body_ of the
`jai` dependency:

```xml

<target name="jai"
  description="load Java Advanced Imaging"
  depends="init">
  <!--    <f2 project="javax.media" archive="jai-core" id="jboss"-->
  <!--        repository="https://repository.jboss.org/nexus/content/groups/public/"/>-->
  <!--    <f2 project="com.sun.media" archive="jai-codec" id="jboss"-->
  <!--        repository="https://repository.jboss.org/nexus/content/groups/public/"/>-->
</target>
```

Then re-run the command in step 3 above (`ant -f fetch.xml -Ddest=optional`). Now you should see

```text
BUILD SUCCESSFUL
Total time: 0 seconds
```

4. Run `ant` in the project and you should see:

```text
BUILD SUCCESSFUL
Total time: 7 seconds
```

## Writing Tests for Apache Ant

1. Add the following xml snippet (I put this at the end of the `build.xml` above the `</project>`
   tag). I chose to go with the embedded approach.

```xml

<project>

  <!-- The rest of the ant build script is above here-->

  <!-- The location to the `dcover` executable -->
  <property name="dcover-executable"
    value="/path/to/dcover"/>

  <path id="test.classpath">
    <pathelement path="${build.classes}"/>
    <path refid="classpath"/>
    <path refid="tests-classpath"/>
  </path>

  <property name="test.dir" value="src/tests/junit"/>

  <target name="dcover-create" depends="compile">
    <exec executable="${dcover-executable}">
      <arg value="create"/>
      <arg value="--classpath=${toString:test.classpath}"/>
      <arg value="--batch"/>
      <arg value="--test-output-dir=${test.dir}"/>
      <arg value="--testing-framework=junit-4"/>
      <arg value="org.apache.tools.ant.util"/>
    </exec>
  </target>
</project>
```

2. Once that's done, you should be able to run `ant dcover-create`, 20mins (or so) later, you should
   have plenty of tests (about 1,500).
3. When the tests have been created, running `ant junit-batch` will run the freshly created unit
   tests
   along-side the projects existing tests.

## JDK Related Problems

Take care to run `dcover` on a JVM that the project is designed to be built against. In this case
it should be run in a 1.8 JVM (and not on 11). If you don't when you run the `ant junit-batch`
command you'll have the following issues.

### `IOStream.readAllBytes`

The following tests all call the `readAllBytes` method, which isn't present on JDKs prior to 9.

* ConcatFileInputStreamDiffblueTest.java
* ConcatResourceInputStreamDiffblueTest.java
* FileUtilsDiffblueTest.java
* LeadPipeInputStreamDiffblueTest.java
* ReaderInputStreamDiffblueTest.java

```text
error: cannot find symbol
  assertEquals(0, readerInputStream.readAllBytes().length);
                                    ^
```

### `PermissionsUtils` Test Failure

```java
public class PermissionUtilsDiffblueTest {

  /** Method under test: {@link PermissionUtils#getPermissions(Resource, Function)} */
  @Test
  public void testGetPermissions2() throws IOException {
    // Arrange and Act
    Set<PosixFilePermission> actualPermissions =
        PermissionUtils.getPermissions(
            new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()), null);

    // Assert
    assertEquals(3, actualPermissions.size());
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_WRITE));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_READ));
    assertTrue(actualPermissions.contains(PosixFilePermission.OWNER_EXECUTE));
  }
}
```

This fails because the size of the `actualPermissions` is 7 and not 3.

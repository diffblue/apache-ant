package org.apache.tools.ant.util.depend.bcel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class FullAnalyzerDiffblueTest {
  /**
  * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
  */
  @Test
  public void testDetermineDependencies() {
    // Arrange
    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertTrue(stringList.isEmpty());
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies2() {
    // Arrange
    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies3() {
    // Arrange
    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addClassPath(new Path(new Project()));
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies4() {
    // Arrange
    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addClassPath(new Path(new Project()));
    fullAnalyzer.addClassPath(new Path(new Project()));
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies5() {
    // Arrange
    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addRootClass(".class");
    fullAnalyzer.addClassPath(new Path(new Project()));
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(2, stringList.size());
    assertEquals("Class Name", stringList.get(0));
    assertEquals(".class", stringList.get(1));
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies6() {
    // Arrange
    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addClassPath(new Path(new Project(), ".class"));
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies7() throws BuildException {
    // Arrange
    Path path = new Path(new Project());
    path.add(new Path(new Project()));

    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addClassPath(path);
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link FullAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies8() {
    // Arrange
    Path path = new Path(new Project());
    path.addJavaRuntime();

    FullAnalyzer fullAnalyzer = new FullAnalyzer();
    fullAnalyzer.addClassPath(path);
    fullAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    fullAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link FullAnalyzer#supportsFileDependencies()}
   */
  @Test
  public void testSupportsFileDependencies() {
    // Arrange, Act and Assert
    assertTrue((new FullAnalyzer()).supportsFileDependencies());
  }
}


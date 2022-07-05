package org.apache.tools.ant.util.depend.bcel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class AncestorAnalyzerDiffblueTest {
  /**
  * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
  */
  @Test
  public void testDetermineDependencies() {
    // Arrange
    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertTrue(stringList.isEmpty());
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies2() {
    // Arrange
    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies3() {
    // Arrange
    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addClassPath(new Path(new Project()));
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies4() {
    // Arrange
    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addClassPath(new Path(new Project()));
    ancestorAnalyzer.addClassPath(new Path(new Project()));
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies5() {
    // Arrange
    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addRootClass(".class");
    ancestorAnalyzer.addClassPath(new Path(new Project()));
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(2, stringList.size());
    assertEquals("Class Name", stringList.get(0));
    assertEquals(".class", stringList.get(1));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies6() {
    // Arrange
    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addClassPath(new Path(new Project(), ".class"));
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies7() throws BuildException {
    // Arrange
    Path path = new Path(new Project());
    path.add(new Path(new Project()));

    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addClassPath(path);
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies8() {
    // Arrange
    Path path = new Path(new Project());
    path.addJavaRuntime();

    AncestorAnalyzer ancestorAnalyzer = new AncestorAnalyzer();
    ancestorAnalyzer.addClassPath(path);
    ancestorAnalyzer.addRootClass("Class Name");
    Vector<File> fileList = new Vector<>();
    Vector<String> stringList = new Vector<>();

    // Act
    ancestorAnalyzer.determineDependencies(fileList, stringList);

    // Assert
    assertTrue(fileList.isEmpty());
    assertEquals(1, stringList.size());
    assertEquals("Class Name", stringList.get(0));
  }

  /**
   * Method under test: {@link AncestorAnalyzer#supportsFileDependencies()}
   */
  @Test
  public void testSupportsFileDependencies() {
    // Arrange, Act and Assert
    assertTrue((new AncestorAnalyzer()).supportsFileDependencies());
  }
}


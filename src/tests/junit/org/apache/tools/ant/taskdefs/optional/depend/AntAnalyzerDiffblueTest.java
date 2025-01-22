package org.apache.tools.ant.taskdefs.optional.depend;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.util.Vector;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class AntAnalyzerDiffblueTest {
  /**
   * Test {@link AntAnalyzer#determineDependencies(Vector, Vector)}.
   * <p>
   * Method under test: {@link AntAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project(), ".class"));
    antAnalyzer.addRootClass("Class Name");
    Vector<File> files = new Vector<>();
    Vector<String> classes = new Vector<>();

    // Act
    antAnalyzer.determineDependencies(files, classes);

    // Assert
    assertEquals(1, classes.size());
    assertEquals("Class Name", classes.get(0));
  }

  /**
   * Test {@link AntAnalyzer#determineDependencies(Vector, Vector)}.
   * <p>
   * Method under test: {@link AntAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies2() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project(), "."));
    antAnalyzer.addRootClass("Class Name");
    Vector<File> files = new Vector<>();
    Vector<String> classes = new Vector<>();

    // Act
    antAnalyzer.determineDependencies(files, classes);

    // Assert
    assertEquals(1, classes.size());
    assertEquals("Class Name", classes.get(0));
  }

  /**
   * Test {@link AntAnalyzer#determineDependencies(Vector, Vector)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addClassPath {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies_givenAntAnalyzerAddClassPathPathWithProjectIsProject() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project()));
    antAnalyzer.addRootClass("Class Name");
    Vector<File> files = new Vector<>();
    Vector<String> classes = new Vector<>();

    // Act
    antAnalyzer.determineDependencies(files, classes);

    // Assert
    assertEquals(1, classes.size());
    assertEquals("Class Name", classes.get(0));
  }

  /**
   * Test {@link AntAnalyzer#determineDependencies(Vector, Vector)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addSourcePath {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies_givenAntAnalyzerAddSourcePathSystemBootClasspath() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(Path.systemBootClasspath);
    antAnalyzer.addClassPath(new Path(new Project(), ".class"));
    antAnalyzer.addRootClass("Class Name");
    Vector<File> files = new Vector<>();
    Vector<String> classes = new Vector<>();

    // Act
    antAnalyzer.determineDependencies(files, classes);

    // Assert
    assertEquals(1, classes.size());
    assertEquals("Class Name", classes.get(0));
  }

  /**
   * Test {@link AntAnalyzer#determineDependencies(Vector, Vector)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor).</li>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link Vector#Vector()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies_givenAntAnalyzer_whenVector_thenVectorEmpty() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    Vector<File> files = new Vector<>();
    Vector<String> classes = new Vector<>();

    // Act
    antAnalyzer.determineDependencies(files, classes);

    // Assert that nothing has changed
    assertTrue(classes.isEmpty());
  }

  /**
   * Test {@link AntAnalyzer#determineDependencies(Vector, Vector)}.
   * <ul>
   *   <li>Then {@link Vector#Vector()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntAnalyzer#determineDependencies(Vector, Vector)}
   */
  @Test
  public void testDetermineDependencies_thenVectorSizeIsOne() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addRootClass("Class Name");
    Vector<File> files = new Vector<>();
    Vector<String> classes = new Vector<>();

    // Act
    antAnalyzer.determineDependencies(files, classes);

    // Assert
    assertEquals(1, classes.size());
    assertEquals("Class Name", classes.get(0));
  }

  /**
   * Test {@link AntAnalyzer#supportsFileDependencies()}.
   * <p>
   * Method under test: {@link AntAnalyzer#supportsFileDependencies()}
   */
  @Test
  public void testSupportsFileDependencies() {
    // Arrange, Act and Assert
    assertTrue((new AntAnalyzer()).supportsFileDependencies());
  }
}

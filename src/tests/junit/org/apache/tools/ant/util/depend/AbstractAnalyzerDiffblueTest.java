package org.apache.tools.ant.util.depend;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.depend.AntAnalyzer;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PropertySet;
import org.junit.Test;

public class AbstractAnalyzerDiffblueTest {
  /**
  * Method under test: {@link AbstractAnalyzer#addClassPath(Path)}
  */
  @Test
  public void testAddClassPath() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();

    // Act
    antAnalyzer.addClassPath(null);

    // Assert that nothing has changed
    assertTrue(antAnalyzer.isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#addClassPath(Path)}
   */
  @Test
  public void testAddClassPath2() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    Path path = new Path(null);

    // Act
    antAnalyzer.addClassPath(path);

    // Assert
    assertNull(path.getProject());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#addRootClass(String)}
   */
  @Test
  public void testAddRootClass() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();

    // Act
    antAnalyzer.addRootClass(null);

    // Assert that nothing has changed
    assertTrue(antAnalyzer.isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#addRootClass(String)}
   */
  @Test
  public void testAddRootClass2() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addRootClass("Class Name");

    // Act
    antAnalyzer.addRootClass("Class Name");

    // Assert that nothing has changed
    assertTrue(antAnalyzer.isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#addSourcePath(Path)}
   */
  @Test
  public void testAddSourcePath() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();

    // Act
    antAnalyzer.addSourcePath(null);

    // Assert that nothing has changed
    assertTrue(antAnalyzer.isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#addSourcePath(Path)}
   */
  @Test
  public void testAddSourcePath2() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    Path path = new Path(null);

    // Act
    antAnalyzer.addSourcePath(path);

    // Assert
    assertNull(path.getProject());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#config(String, Object)}
   */
  @Test
  public void testConfig() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();

    // Act
    antAnalyzer.config("Name", "Info");

    // Assert that nothing has changed
    assertTrue(antAnalyzer.isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer() throws IOException {
    // Arrange, Act and Assert
    assertNull((new AntAnalyzer()).getClassContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer2() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer3() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project()));
    antAnalyzer.addClassPath(new Path(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer4() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project(), ".class"));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer5() throws IOException {
    // Arrange
    Path path = new Path(new Project());
    path.addJavaRuntime();

    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(path);

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer6() throws IOException {
    // Arrange
    Path path = new Path(new Project());
    path.add(new PropertySet());

    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(path);

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer() throws IOException {
    // Arrange, Act and Assert
    assertNull((new AntAnalyzer()).getSourceContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer2() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new Path(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer3() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new Path(new Project()));
    antAnalyzer.addSourcePath(new Path(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer4() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new Path(new Project(), ".java"));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer5() throws IOException {
    // Arrange
    Path path = new Path(new Project());
    path.addJavaRuntime();

    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(path);

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer6() throws IOException {
    // Arrange
    Path path = new Path(new Project());
    path.add(new PropertySet());

    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(path);

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Method under test: {@link AbstractAnalyzer#isClosureRequired()}
   */
  @Test
  public void testIsClosureRequired() {
    // Arrange, Act and Assert
    assertTrue((new AntAnalyzer()).isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#isClosureRequired()}
   */
  @Test
  public void testIsClosureRequired2() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.setClosure(false);

    // Act and Assert
    assertFalse(antAnalyzer.isClosureRequired());
  }

  /**
   * Method under test: {@link AbstractAnalyzer#setClosure(boolean)}
   */
  @Test
  public void testSetClosure() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();

    // Act
    antAnalyzer.setClosure(true);

    // Assert
    assertTrue(antAnalyzer.isClosureRequired());
  }
}


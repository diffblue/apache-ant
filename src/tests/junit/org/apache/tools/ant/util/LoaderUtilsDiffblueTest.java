package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import junit.runner.TestCaseClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class LoaderUtilsDiffblueTest {
  /**
  * Method under test: {@link LoaderUtils#classExists(ClassLoader, String)}
  */
  @Test
  public void testClassExists() {
    // Arrange, Act and Assert
    assertFalse(LoaderUtils.classExists(new TestCaseClassLoader(), "Class Name"));
    assertFalse(LoaderUtils.classExists(new TestCaseClassLoader(), "foo"));
  }

  /**
   * Method under test: {@link LoaderUtils#classExists(ClassLoader, String)}
   */
  @Test
  public void testClassExists2() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());

    // Act and Assert
    assertFalse(LoaderUtils.classExists(new SplitClassLoader(parent, path, new Project(), new String[]{".class"}),
        "Class Name"));
  }

  /**
   * Method under test: {@link LoaderUtils#classExists(ClassLoader, String)}
   */
  @Test
  public void testClassExists3() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition(".class", Object.class);
    TestCaseClassLoader parent = new TestCaseClassLoader();

    // Act and Assert
    assertFalse(LoaderUtils.classExists(
        new SplitClassLoader(parent, new Path(new Project()), project, new String[]{".class"}), "Class Name"));
  }

  /**
   * Method under test: {@link LoaderUtils#classNameToResource(String)}
   */
  @Test
  public void testClassNameToResource() {
    // Arrange, Act and Assert
    assertEquals("Class Name.class", LoaderUtils.classNameToResource("Class Name"));
  }

  /**
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource() {
    // Arrange, Act and Assert
    assertNull(LoaderUtils.getResourceSource(new TestCaseClassLoader(), "Resource"));
    assertNull(LoaderUtils.getResourceSource(null, "foo"));
  }

  /**
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource2() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());

    // Act and Assert
    assertNull(LoaderUtils.getResourceSource(
        new SplitClassLoader(parent, path, new Project(), new String[]{"Split Classes"}), "Resource"));
  }

  /**
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource3() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("Couldn't load Resource ", Object.class);
    TestCaseClassLoader parent = new TestCaseClassLoader();

    // Act and Assert
    assertNull(LoaderUtils.getResourceSource(
        new SplitClassLoader(parent, new Path(new Project()), project, new String[]{"Split Classes"}), "Resource"));
  }
}


package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import junit.runner.TestCaseClassLoader;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class SplitClassLoaderDiffblueTest {
  /**
  * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
  */
  @Test
  public void testConstructor() {
    // Arrange
    TestCaseClassLoader testCaseClassLoader = new TestCaseClassLoader();
    Path path = new Path(new Project());

    // Act
    SplitClassLoader actualSplitClassLoader = new SplitClassLoader(testCaseClassLoader, path, new Project(),
        new String[]{"Split Classes"});

    // Assert
    assertEquals("", actualSplitClassLoader.getClasspath());
    assertSame(testCaseClassLoader, actualSplitClassLoader.getConfiguredParent());
    Project project = path.getProject();
    Hashtable<String, Object> userProperties = project.getUserProperties();
    assertTrue(userProperties.isEmpty());
    assertEquals(userProperties, project.getTaskDefinitions());
    Hashtable<String, Target> expectedInheritedProperties = project.getTargets();
    assertEquals(expectedInheritedProperties, project.getInheritedProperties());
    assertNull(project.getDescription());
  }

  /**
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testConstructor2() {
    // Arrange
    TestCaseClassLoader testCaseClassLoader = new TestCaseClassLoader();
    Path path = new Path(null);

    // Act
    SplitClassLoader actualSplitClassLoader = new SplitClassLoader(testCaseClassLoader, path, new Project(),
        new String[]{"Split Classes"});

    // Assert
    assertEquals("", actualSplitClassLoader.getClasspath());
    assertSame(testCaseClassLoader, actualSplitClassLoader.getConfiguredParent());
  }

  /**
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testConstructor3() {
    // Arrange
    TestCaseClassLoader testCaseClassLoader = new TestCaseClassLoader();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    Path path = new Path(project);

    // Act
    SplitClassLoader actualSplitClassLoader = new SplitClassLoader(testCaseClassLoader, path, new Project(),
        new String[]{"Split Classes"});

    // Assert
    assertEquals("", actualSplitClassLoader.getClasspath());
    assertSame(testCaseClassLoader, actualSplitClassLoader.getConfiguredParent());
    Project project1 = path.getProject();
    Hashtable<String, Object> userProperties = project1.getUserProperties();
    assertTrue(userProperties.isEmpty());
    assertEquals(userProperties, project1.getTaskDefinitions());
    Hashtable<String, Target> expectedInheritedProperties = project1.getTargets();
    assertEquals(expectedInheritedProperties, project1.getInheritedProperties());
    assertNull(project1.getDescription());
  }

  /**
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testConstructor4() {
    // Arrange
    TestCaseClassLoader testCaseClassLoader = new TestCaseClassLoader();

    Project project = new Project();
    project.addDataTypeDefinition("ignore", Object.class);
    project.addBuildListener(new AntClassLoader());
    Path path = new Path(project);

    // Act
    SplitClassLoader actualSplitClassLoader = new SplitClassLoader(testCaseClassLoader, path, new Project(),
        new String[]{"Split Classes"});

    // Assert
    assertEquals("", actualSplitClassLoader.getClasspath());
    assertSame(testCaseClassLoader, actualSplitClassLoader.getConfiguredParent());
  }

  /**
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testConstructor5() throws BuildException {
    // Arrange
    TestCaseClassLoader testCaseClassLoader = new TestCaseClassLoader();

    Project project = new Project();
    project.addTarget("ignore", new Target());
    project.addBuildListener(new AntClassLoader());
    Path path = new Path(project);

    // Act
    SplitClassLoader actualSplitClassLoader = new SplitClassLoader(testCaseClassLoader, path, new Project(),
        new String[]{"Split Classes"});

    // Assert
    assertEquals("", actualSplitClassLoader.getClasspath());
    assertSame(testCaseClassLoader, actualSplitClassLoader.getConfiguredParent());
    Project project1 = path.getProject();
    Hashtable<String, Object> userProperties = project1.getUserProperties();
    assertTrue(userProperties.isEmpty());
    Hashtable<String, Class<?>> taskDefinitions = project1.getTaskDefinitions();
    assertEquals(userProperties, taskDefinitions);
    assertEquals(taskDefinitions, project1.getInheritedProperties());
    assertNull(project1.getDescription());
  }
}


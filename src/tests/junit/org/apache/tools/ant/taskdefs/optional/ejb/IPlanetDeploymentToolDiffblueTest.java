package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class IPlanetDeploymentToolDiffblueTest {
  /**
   * Test {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return Name is {@code Base Name.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenJavaLangObject_thenReturnNameIsBaseNameJar() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Then return Name is {@code Base Name.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_thenReturnNameIsBaseNameJar() {
    // Arrange
    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(new TaskAdapter());

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Then return Name is {@code Base Name.jar}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_thenReturnNameIsBaseNameJar2() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    IPlanetDeploymentTool iPlanetDeploymentTool = new IPlanetDeploymentTool();
    iPlanetDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = iPlanetDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link IPlanetDeploymentTool}
   *   <li>{@link IPlanetDeploymentTool#setDebug(boolean)}
   *   <li>{@link IPlanetDeploymentTool#setIashome(File)}
   *   <li>{@link IPlanetDeploymentTool#setKeepgenerated(boolean)}
   *   <li>{@link IPlanetDeploymentTool#setSuffix(String)}
   *   <li>{@link IPlanetDeploymentTool#getPublicId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    IPlanetDeploymentTool actualIPlanetDeploymentTool = new IPlanetDeploymentTool();
    actualIPlanetDeploymentTool.setDebug(true);
    actualIPlanetDeploymentTool.setIashome(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualIPlanetDeploymentTool.setKeepgenerated(true);
    actualIPlanetDeploymentTool.setSuffix("Jar Suffix");
    String actualPublicId = actualIPlanetDeploymentTool.getPublicId();

    // Assert
    assertNull(actualIPlanetDeploymentTool.getDestDir());
    assertNull(actualPublicId);
    assertNull(actualIPlanetDeploymentTool.getTask());
    assertNull(actualIPlanetDeploymentTool.getConfig());
  }
}

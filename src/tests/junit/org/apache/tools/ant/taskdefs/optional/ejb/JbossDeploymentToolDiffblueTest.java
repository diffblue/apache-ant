package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JbossDeploymentToolDiffblueTest {
  /**
   * Test {@link JbossDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link JbossDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile() {
    // Arrange
    EjbJar task = new EjbJar();
    task.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JbossDeploymentTool jbossDeploymentTool = new JbossDeploymentTool();
    jbossDeploymentTool.setDestdir(null);
    jbossDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = jbossDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertTrue(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link JbossDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link JbossDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile2() {
    // Arrange
    EjbJar task = new EjbJar();
    task.setDestdir(null);

    JbossDeploymentTool jbossDeploymentTool = new JbossDeploymentTool();
    jbossDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jbossDeploymentTool.setTask(task);

    // Act
    File actualVendorOutputJarFile = jbossDeploymentTool.getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertTrue(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link JbossDeploymentTool#getVendorOutputJarFile(String)}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor) Destdir is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JbossDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile_givenEjbJarDestdirIsNull_thenThrowBuildException() {
    // Arrange
    EjbJar task = new EjbJar();
    task.setDestdir(null);

    JbossDeploymentTool jbossDeploymentTool = new JbossDeploymentTool();
    jbossDeploymentTool.setDestdir(null);
    jbossDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jbossDeploymentTool.getVendorOutputJarFile("Base Name"));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link JbossDeploymentTool}
   *   <li>{@link JbossDeploymentTool#setSuffix(String)}
   *   <li>{@link JbossDeploymentTool#validateConfigured()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange and Act
    JbossDeploymentTool actualJbossDeploymentTool = new JbossDeploymentTool();
    actualJbossDeploymentTool.setSuffix("In String");
    actualJbossDeploymentTool.validateConfigured();

    // Assert
    assertNull(actualJbossDeploymentTool.getDestDir());
    assertNull(actualJbossDeploymentTool.getTask());
    assertNull(actualJbossDeploymentTool.getConfig());
  }
}

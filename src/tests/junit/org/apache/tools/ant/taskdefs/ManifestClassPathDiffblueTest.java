package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class ManifestClassPathDiffblueTest {
  /**
   * Test {@link ManifestClassPath#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    ManifestClassPath manifestClassPath = new ManifestClassPath();
    manifestClassPath.setProject(project);
    manifestClassPath.setJarFile(Copy.NULL_FILE_PLACEHOLDER);
    manifestClassPath.setProperty("Missing 'property' attribute!");

    // Act and Assert
    assertThrows(BuildException.class, () -> manifestClassPath.execute());
  }

  /**
   * Test {@link ManifestClassPath#execute()}.
   * <ul>
   *   <li>Given {@link ManifestClassPath} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#execute()}
   */
  @Test
  public void testExecute_givenManifestClassPathProjectIsProject_thenThrowBuildException() {
    // Arrange
    ManifestClassPath manifestClassPath = new ManifestClassPath();
    manifestClassPath.setProject(new Project());
    manifestClassPath.setJarFile(Copy.NULL_FILE_PLACEHOLDER);
    manifestClassPath.setProperty("Missing 'property' attribute!");

    // Act and Assert
    assertThrows(BuildException.class, () -> manifestClassPath.execute());
  }

  /**
   * Test {@link ManifestClassPath#execute()}.
   * <ul>
   *   <li>Given {@link ManifestClassPath} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#execute()}
   */
  @Test
  public void testExecute_givenManifestClassPath_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ManifestClassPath()).execute());
  }

  /**
   * Test {@link ManifestClassPath#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ManifestClassPath manifestClassPath = new ManifestClassPath();
    manifestClassPath.setProject(project);
    manifestClassPath.setJarFile(Copy.NULL_FILE_PLACEHOLDER);
    manifestClassPath.setProperty("Missing 'property' attribute!");

    // Act and Assert
    assertThrows(BuildException.class, () -> manifestClassPath.execute());
  }

  /**
   * Test {@link ManifestClassPath#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#execute()}
   */
  @Test
  public void testExecute_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    ManifestClassPath manifestClassPath = new ManifestClassPath();
    manifestClassPath.setProject(project);
    manifestClassPath.setJarFile(Copy.NULL_FILE_PLACEHOLDER);
    manifestClassPath.setProperty("Missing 'property' attribute!");

    // Act and Assert
    assertThrows(BuildException.class, () -> manifestClassPath.execute());
  }

  /**
   * Test {@link ManifestClassPath#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() {
    // Arrange
    ManifestClassPath manifestClassPath = new ManifestClassPath();
    manifestClassPath.setProperty("Missing 'property' attribute!");

    // Act and Assert
    assertThrows(BuildException.class, () -> manifestClassPath.execute());
  }

  /**
   * Test {@link ManifestClassPath#setMaxParentLevels(int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestClassPath#setMaxParentLevels(int)}
   */
  @Test
  public void testSetMaxParentLevels_whenMinusOne_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ManifestClassPath()).setMaxParentLevels(-1));
  }

  /**
   * Test new {@link ManifestClassPath} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ManifestClassPath}
   */
  @Test
  public void testNewManifestClassPath() {
    // Arrange and Act
    ManifestClassPath actualManifestClassPath = new ManifestClassPath();

    // Assert
    Location location = actualManifestClassPath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualManifestClassPath.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualManifestClassPath.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualManifestClassPath.getTaskName());
    assertNull(actualManifestClassPath.getTaskType());
    assertNull(actualManifestClassPath.getProject());
    assertNull(actualManifestClassPath.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualManifestClassPath, runtimeConfigurableWrapper.getProxy());
  }
}

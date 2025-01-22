package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class RenameDiffblueTest {
  /**
   * Test {@link Rename#execute()}.
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("DEPRECATED - The rename task is deprecated.  Use move instead."));

    Rename rename = new Rename();
    rename.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("dest attribute is required", typeClass);
    project.addBuildListener(new AntClassLoader());

    Rename rename = new Rename();
    rename.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rename rename = new Rename();
    rename.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Rename rename = new Rename();
    rename.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerRecorder_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    Rename rename = new Rename();
    rename.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@link Rename} (default constructor) Dest is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenRenameDestIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Rename rename = new Rename();
    rename.setDest(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@link Rename} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenRenameProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Rename rename = new Rename();
    rename.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> rename.execute());
  }

  /**
   * Test {@link Rename#execute()}.
   * <ul>
   *   <li>Given {@link Rename} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rename#execute()}
   */
  @Test
  public void testExecute_givenRename_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Rename()).execute());
  }

  /**
   * Test new {@link Rename} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Rename}
   */
  @Test
  public void testNewRename() {
    // Arrange and Act
    Rename actualRename = new Rename();

    // Assert
    Location location = actualRename.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRename.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualRename.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualRename.getTaskName());
    assertNull(actualRename.getTaskType());
    assertNull(actualRename.getProject());
    assertNull(actualRename.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualRename, runtimeConfigurableWrapper.getProxy());
  }
}

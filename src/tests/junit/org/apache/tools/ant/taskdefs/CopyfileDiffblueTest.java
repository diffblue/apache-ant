package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class CopyfileDiffblueTest {
  /**
   * Test {@link Copyfile#execute()}.
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Copyfile copyfile = new Copyfile();
    copyfile.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("DEPRECATED - The copyfile task is deprecated.  Use copy instead."));

    Copyfile copyfile = new Copyfile();
    copyfile.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@link Copyfile} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenCopyfileProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Copyfile copyfile = new Copyfile();
    copyfile.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@link Copyfile} (default constructor) Src is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenCopyfileSrcIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Copyfile copyfile = new Copyfile();
    copyfile.setSrc(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@link Copyfile} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenCopyfile_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Copyfile()).execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("The src attribute must be present.", typeClass);
    project.addBuildListener(new AntClassLoader());

    Copyfile copyfile = new Copyfile();
    copyfile.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Copyfile copyfile = new Copyfile();
    copyfile.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Copyfile copyfile = new Copyfile();
    copyfile.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test {@link Copyfile#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copyfile#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerRecorder_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    Copyfile copyfile = new Copyfile();
    copyfile.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyfile.execute());
  }

  /**
   * Test new {@link Copyfile} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Copyfile}
   */
  @Test
  public void testNewCopyfile() {
    // Arrange and Act
    Copyfile actualCopyfile = new Copyfile();

    // Assert
    Location location = actualCopyfile.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCopyfile.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCopyfile.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCopyfile.getTaskName());
    assertNull(actualCopyfile.getTaskType());
    assertNull(actualCopyfile.getProject());
    assertNull(actualCopyfile.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCopyfile, runtimeConfigurableWrapper.getProxy());
  }
}

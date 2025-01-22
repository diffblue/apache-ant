package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class CopydirDiffblueTest {
  /**
   * Test {@link Copydir#execute()}.
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Copydir copydir = new Copydir();
    copydir.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("DEPRECATED - The copydir task is deprecated.  Use copy instead."));

    Copydir copydir = new Copydir();
    copydir.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@link Copydir} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenCopydirProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Copydir copydir = new Copydir();
    copydir.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@link Copydir} (default constructor) Src is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenCopydirSrcIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Copydir copydir = new Copydir();
    copydir.setSrc(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@link Copydir} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenCopydir_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Copydir()).execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("src attribute must be set!", typeClass);
    project.addBuildListener(new AntClassLoader());

    Copydir copydir = new Copydir();
    copydir.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Copydir copydir = new Copydir();
    copydir.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Copydir copydir = new Copydir();
    copydir.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test {@link Copydir#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copydir#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerRecorder_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    Copydir copydir = new Copydir();
    copydir.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copydir.execute());
  }

  /**
   * Test new {@link Copydir} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Copydir}
   */
  @Test
  public void testNewCopydir() {
    // Arrange and Act
    Copydir actualCopydir = new Copydir();

    // Assert
    assertNull(actualCopydir.getDescription());
    assertNull(actualCopydir.getTaskName());
    assertNull(actualCopydir.getTaskType());
    assertNull(actualCopydir.getProject());
    assertNull(actualCopydir.getOwningTarget());
    assertFalse(actualCopydir.hasSelectors());
  }
}

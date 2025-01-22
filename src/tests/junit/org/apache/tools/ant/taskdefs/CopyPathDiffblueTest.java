package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class CopyPathDiffblueTest {
  /**
   * Test {@link CopyPath#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenCopyPathAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    CopyPath copyPath = new CopyPath();
    copyPath.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.add(new CutDirsMapper()));
  }

  /**
   * Test {@link CopyPath#createPath()}.
   * <p>
   * Method under test: {@link CopyPath#createPath()}
   */
  @Test
  public void testCreatePath() {
    // Arrange and Act
    Path actualCreatePathResult = (new CopyPath()).createPath();

    // Assert
    Location location = actualCreatePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePathResult.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(actualCreatePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreatePathResult.size());
    assertFalse(actualCreatePathResult.isReference());
    assertTrue(actualCreatePathResult.isEmpty());
  }

  /**
   * Test {@link CopyPath#validateAttributes()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyPath() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CopyPath()).validateAttributes());
  }

  /**
   * Test {@link CopyPath#validateAttributes()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyPathAddCutDirsMapper() throws BuildException {
    // Arrange
    CopyPath copyPath = new CopyPath();
    copyPath.add(new CutDirsMapper());
    copyPath.setDestDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.validateAttributes());
  }

  /**
   * Test {@link CopyPath#validateAttributes()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor) DestDir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyPathDestDirIsNull_file_placeholder() throws BuildException {
    // Arrange
    CopyPath copyPath = new CopyPath();
    copyPath.setDestDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.validateAttributes());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry(String.join("", "This task should have never been ",
        System.getProperty("jdk.debug"),
        "d and was obsoleted by ResourceCollection support in <copy> available since Ant 1.7.0.  Don't" + " use it.")));

    CopyPath copyPath = new CopyPath();
    copyPath.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.execute());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute_givenCopyPathAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    CopyPath copyPath = new CopyPath();
    copyPath.add(new CutDirsMapper());
    copyPath.setDestDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.execute());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor) DestDir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute_givenCopyPathDestDirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    CopyPath copyPath = new CopyPath();
    copyPath.setDestDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.execute());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute_givenCopyPathProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    CopyPath copyPath = new CopyPath();
    copyPath.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.execute());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <ul>
   *   <li>Given {@link CopyPath} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute_givenCopyPath_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CopyPath()).execute());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CopyPath copyPath = new CopyPath();
    copyPath.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.execute());
  }

  /**
   * Test {@link CopyPath#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addDataTypeDefinition {@link CopyPath#ERROR_NO_DESTDIR} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CopyPath#execute()}
   */
  @Test
  public void testExecute_givenProjectAddDataTypeDefinitionError_no_destdirAndObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(CopyPath.ERROR_NO_DESTDIR, typeClass);
    project.addBuildListener(new AntClassLoader());

    CopyPath copyPath = new CopyPath();
    copyPath.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> copyPath.execute());
  }

  /**
   * Test new {@link CopyPath} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CopyPath}
   */
  @Test
  public void testNewCopyPath() {
    // Arrange and Act
    CopyPath actualCopyPath = new CopyPath();

    // Assert
    Location location = actualCopyPath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCopyPath.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCopyPath.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCopyPath.getTaskName());
    assertNull(actualCopyPath.getTaskType());
    assertNull(actualCopyPath.getProject());
    assertNull(actualCopyPath.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCopyPath, runtimeConfigurableWrapper.getProxy());
  }
}

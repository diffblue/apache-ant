package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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
import org.apache.tools.ant.taskdefs.Available.FileDir;
import org.apache.tools.ant.taskdefs.ExecuteOn.FileDirBoth;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class AvailableDiffblueTest {
  /**
   * Test {@link Available#createClasspath()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenAvailableProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Available available = new Available();
    Project project = new Project();
    available.setProject(project);

    // Act and Assert
    assertSame(project, available.createClasspath().getProject());
  }

  /**
   * Test {@link Available#createClasspath()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenAvailable_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new Available()).createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link Available#createFilepath()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#createFilepath()}
   */
  @Test
  public void testCreateFilepath_givenAvailableProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Available available = new Available();
    Project project = new Project();
    available.setProject(project);

    // Act and Assert
    assertSame(project, available.createFilepath().getProject());
  }

  /**
   * Test {@link Available#createFilepath()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#createFilepath()}
   */
  @Test
  public void testCreateFilepath_givenAvailable_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateFilepathResult = (new Available()).createFilepath();

    // Assert
    Location location = actualCreateFilepathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateFilepathResult.getDescription());
    assertNull(actualCreateFilepathResult.getProject());
    assertNull(actualCreateFilepathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateFilepathResult.size());
    assertFalse(actualCreateFilepathResult.isReference());
    assertTrue(actualCreateFilepathResult.isEmpty());
  }

  /**
   * Test {@link Available#execute()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Type is {@link FileDir} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#execute()}
   */
  @Test
  public void testExecute_givenAvailableTypeIsFileDir_thenThrowBuildException() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setType(new FileDir());
    available.setResource("");
    available.setProperty("property attribute is required");

    // Act and Assert
    assertThrows(BuildException.class, () -> available.execute());
  }

  /**
   * Test {@link Available#execute()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#execute()}
   */
  @Test
  public void testExecute_givenAvailable_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Available()).execute());
  }

  /**
   * Test {@link Available#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setProperty("property attribute is required");

    // Act and Assert
    assertThrows(BuildException.class, () -> available.execute());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenAvailableProjectIsProject_thenReturnFalse() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setProject(new Project());
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Resource is {@code At least one of (classname|file|resource) is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenAvailableResourceIsAtLeastOneOfClassnameFileResourceIsRequired() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setResource("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Resource is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenAvailableResourceIsEmptyString_thenReturnTrue() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setResource("");

    // Act and Assert
    assertTrue(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Type is {@link FileDir} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenAvailableTypeIsFileDir_thenThrowBuildException() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setType(new FileDir());
    available.setResource("");

    // Act and Assert
    assertThrows(BuildException.class, () -> available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenAvailable_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Available()).eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Available available = new Available();
    available.setProject(project);
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Available available = new Available();
    available.setIgnoresystemclasses(true);
    available.setProject(project);
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerDefaultLogger_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Available available = new Available();
    available.setProject(project);
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link RecorderEntry#RecorderEntry(String)} with name is {@code available}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerRecorderEntryWithNameIsAvailable() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("available"));

    Available available = new Available();
    available.setProject(project);
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerRecorder_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    Available available = new Available();
    available.setProject(project);
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test {@link Available#eval()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Available#eval()}
   */
  @Test
  public void testEval_thenReturnFalse() throws BuildException {
    // Arrange
    Available available = new Available();
    available.setClassname("At least one of (classname|file|resource) is required");

    // Act and Assert
    assertFalse(available.eval());
  }

  /**
   * Test FileDir getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link FileDir}
   *   <li>{@link FileDir#getValues()}
   * </ul>
   */
  @Test
  public void testFileDirGettersAndSetters() {
    // Arrange and Act
    FileDir actualFileDir = new FileDir();
    String[] actualValues = actualFileDir.getValues();

    // Assert
    assertNull(actualFileDir.getValue());
    assertEquals(-1, actualFileDir.getIndex());
    assertArrayEquals(new String[]{FileDirBoth.FILE, FileDirBoth.DIR}, actualValues);
  }

  /**
   * Test FileDir {@link FileDir#isDir()}.
   * <p>
   * Method under test: {@link FileDir#isDir()}
   */
  @Test
  public void testFileDirIsDir() {
    // Arrange, Act and Assert
    assertFalse((new FileDir()).isDir());
  }

  /**
   * Test FileDir {@link FileDir#isFile()}.
   * <p>
   * Method under test: {@link FileDir#isFile()}
   */
  @Test
  public void testFileDirIsFile() {
    // Arrange, Act and Assert
    assertFalse((new FileDir()).isFile());
  }

  /**
   * Test new {@link Available} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Available}
   */
  @Test
  public void testNewAvailable() {
    // Arrange and Act
    Available actualAvailable = new Available();

    // Assert
    Location location = actualAvailable.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAvailable.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualAvailable.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualAvailable.getTaskName());
    assertNull(actualAvailable.getTaskType());
    assertNull(actualAvailable.getProject());
    assertNull(actualAvailable.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualAvailable, runtimeConfigurableWrapper.getProxy());
  }
}

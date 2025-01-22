package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.OutputStream;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.RedirectorElement;
import org.apache.tools.ant.util.LineOrientedOutputStreamRedirector;
import org.apache.tools.ant.util.NullOutputStream;
import org.junit.Test;

public class ExecTaskDiffblueTest {
  /**
   * Test {@link ExecTask#ExecTask()}.
   * <p>
   * Method under test: {@link ExecTask#ExecTask()}
   */
  @Test
  public void testNewExecTask() {
    // Arrange and Act
    ExecTask actualExecTask = new ExecTask();

    // Assert
    assertNull(actualExecTask.getDescription());
    assertNull(actualExecTask.getTaskName());
    assertNull(actualExecTask.getTaskType());
    assertNull(actualExecTask.getOs());
    assertNull(actualExecTask.getOsFamily());
    assertNull(actualExecTask.getProject());
    assertNull(actualExecTask.getOwningTarget());
    assertNull(actualExecTask.redirectorElement);
    assertFalse(actualExecTask.getResolveExecutable());
    assertFalse(actualExecTask.failOnError);
    assertFalse(actualExecTask.newEnvironment);
  }

  /**
   * Test {@link ExecTask#ExecTask(Task)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#ExecTask(Task)}
   */
  @Test
  public void testNewExecTask_whenTaskAdapter_thenReturnDescriptionIsNull() {
    // Arrange and Act
    ExecTask actualExecTask = new ExecTask(new TaskAdapter());

    // Assert
    assertNull(actualExecTask.getDescription());
    assertNull(actualExecTask.getTaskName());
    assertNull(actualExecTask.getTaskType());
    assertNull(actualExecTask.getOs());
    assertNull(actualExecTask.getOsFamily());
    assertNull(actualExecTask.getProject());
    assertNull(actualExecTask.getOwningTarget());
    assertNull(actualExecTask.redirectorElement);
    assertFalse(actualExecTask.getResolveExecutable());
    assertFalse(actualExecTask.failOnError);
    assertFalse(actualExecTask.newEnvironment);
  }

  /**
   * Test {@link ExecTask#setExecutable(String)}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} Executable is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#setExecutable(String)}
   */
  @Test
  public void testSetExecutable_givenExecTask_when42_thenExecTaskCmdlExecutableIs42() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.setExecutable("42");

    // Assert
    Commandline commandline = execTask.cmdl;
    assertEquals("42", commandline.getExecutable());
    assertEquals(1, commandline.size());
    assertArrayEquals(new String[]{"42"}, commandline.getCommandline());
  }

  /**
   * Test {@link ExecTask#setExecutable(String)}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>When empty string.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#setExecutable(String)}
   */
  @Test
  public void testSetExecutable_givenExecTask_whenEmptyString_thenExecTaskCmdlSizeIsZero() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.setExecutable("");

    // Assert that nothing has changed
    Commandline commandline = execTask.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getCommandline().length);
  }

  /**
   * Test {@link ExecTask#setExecutable(String)}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#setExecutable(String)}
   */
  @Test
  public void testSetExecutable_givenExecTask_whenNull_thenExecTaskCmdlSizeIsZero() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.setExecutable(null);

    // Assert that nothing has changed
    Commandline commandline = execTask.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getCommandline().length);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ExecTask#setDir(File)}
   *   <li>{@link ExecTask#setNewenvironment(boolean)}
   *   <li>{@link ExecTask#setOs(String)}
   *   <li>{@link ExecTask#setResolveExecutable(boolean)}
   *   <li>{@link ExecTask#setSearchPath(boolean)}
   *   <li>{@link ExecTask#setSpawn(boolean)}
   *   <li>{@link ExecTask#setVMLauncher(boolean)}
   *   <li>{@link ExecTask#logFlush()}
   *   <li>{@link ExecTask#getOs()}
   *   <li>{@link ExecTask#getOsFamily()}
   *   <li>{@link ExecTask#getResolveExecutable()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.setDir(Copy.NULL_FILE_PLACEHOLDER);
    execTask.setNewenvironment(true);
    execTask.setOs("Os");
    execTask.setResolveExecutable(true);
    execTask.setSearchPath(true);
    execTask.setSpawn(true);
    execTask.setVMLauncher(true);
    execTask.logFlush();
    String actualOs = execTask.getOs();
    String actualOsFamily = execTask.getOsFamily();

    // Assert
    assertEquals("Os", actualOs);
    assertNull(actualOsFamily);
    assertTrue(execTask.getResolveExecutable());
  }

  /**
   * Test {@link ExecTask#setFailonerror(boolean)}.
   * <p>
   * Method under test: {@link ExecTask#setFailonerror(boolean)}
   */
  @Test
  public void testSetFailonerror() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.setFailonerror(true);

    // Assert
    assertTrue(execTask.failOnError);
  }

  /**
   * Test {@link ExecTask#addConfiguredRedirector(RedirectorElement)}.
   * <ul>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#redirectorElement} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#addConfiguredRedirector(RedirectorElement)}
   */
  @Test
  public void testAddConfiguredRedirector_thenExecTaskRedirectorElementDescriptionIsNull() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.addConfiguredRedirector(new RedirectorElement());

    // Assert
    RedirectorElement redirectorElement = execTask.redirectorElement;
    assertNull(redirectorElement.getDescription());
    assertNull(redirectorElement.getProject());
    assertNull(redirectorElement.getRefid());
    assertFalse(redirectorElement.isReference());
  }

  /**
   * Test {@link ExecTask#addConfiguredRedirector(RedirectorElement)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#addConfiguredRedirector(RedirectorElement)}
   */
  @Test
  public void testAddConfiguredRedirector_thenThrowBuildException() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.addConfiguredRedirector(new RedirectorElement());

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.addConfiguredRedirector(new RedirectorElement()));
  }

  /**
   * Test {@link ExecTask#setOsFamily(String)}.
   * <p>
   * Method under test: {@link ExecTask#setOsFamily(String)}
   */
  @Test
  public void testSetOsFamily() {
    // Arrange
    ExecTask execTask = new ExecTask();

    // Act
    execTask.setOsFamily("Os Family");

    // Assert
    assertEquals("os family", execTask.getOsFamily());
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setResolveExecutable(true);

    // Act
    String actualResolveExecutableResult = execTask.resolveExecutable("..", true);

    // Assert
    assertEquals(Paths.get(System.getProperty("user.home"), "Downloads").toString(), actualResolveExecutableResult);
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code Exec}.</li>
   *   <li>Then return {@code Exec}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_givenExecTaskProjectIsProject_whenExec_thenReturnExec() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals("Exec", execTask.resolveExecutable("Exec", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code Exec}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_givenExecTaskProjectIsProject_whenFalse_thenReturnExec() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals("Exec", execTask.resolveExecutable("Exec", false));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>When {@code Exec}.</li>
   *   <li>Then return {@code Exec}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_givenExecTask_whenExec_thenReturnExec() {
    // Arrange, Act and Assert
    assertEquals("Exec", (new ExecTask()).resolveExecutable("Exec", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Exec}.</li>
   *   <li>Then return {@code Exec}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_givenJavaLangObject_whenExec_thenReturnExec() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Users", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals("Exec", execTask.resolveExecutable("Exec", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code Exec}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_givenProjectAddBuildListenerAntClassLoader_thenReturnExec() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals("Exec", execTask.resolveExecutable("Exec", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then return {@code Exec}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_givenProjectAddBuildListenerDefaultLogger_thenReturnExec() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals("Exec", execTask.resolveExecutable("Exec", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_whenDot_thenReturnPropertyIsUserDir() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), execTask.resolveExecutable(".", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_whenEmptyString_thenReturnPropertyIsUserDir() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), execTask.resolveExecutable("", true));
  }

  /**
   * Test {@link ExecTask#resolveExecutable(String, boolean)}.
   * <ul>
   *   <li>When {@code Users}.</li>
   *   <li>Then return {@code /usr/bin/Users}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#resolveExecutable(String, boolean)}
   */
  @Test
  public void testResolveExecutable_whenUsers_thenReturnUsrBinUsers() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setResolveExecutable(true);

    // Act and Assert
    assertEquals("/usr/bin/Users", execTask.resolveExecutable("Users", true));
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenExecTaskProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenExecTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ExecTask()).execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenExecuteOn_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ExecuteOn()).execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Current OS is ", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link RecorderEntry#RecorderEntry(String)} with name is {@code os.name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerRecorderEntryWithNameIsOsName() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("os.name"));

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.execute());
  }

  /**
   * Test {@link ExecTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerRecorder_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.execute());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecTaskDirIsNull_file_placeholder() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setDir(Copy.NULL_FILE_PLACEHOLDER);
    execTask.setCommand(new Commandline("no executable specified"));

    // Act and Assert
    assertThrows(BuildException.class, () -> execTask.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ExecTask()).checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecuteOnProjectIsProject_thenThrowBuildException() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(new Project());
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) TaskName is {@code execon}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecuteOnTaskNameIsExecon_thenThrowBuildException() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecuteOn_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ExecuteOn()).checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("!! execon is deprecated. Use apply instead. !!", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenProjectAddBuildListenerRecorder() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link RecorderEntry#RecorderEntry(String)} with name is {@code execon}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenProjectAddBuildListenerRecorderEntryWithNameIsExecon() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("execon"));

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setTaskName("execon");

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) OsFamily is {@code windows}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChmodOsFamilyIsWindows_thenReturnFalse() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOsFamily("windows");
    chmod.setOs(null);

    // Act and Assert
    assertFalse(chmod.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChmod_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Chmod()).isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code dos}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsDos_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("dos");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code mac}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsMac_thenReturnTrue() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("mac");
    execTask.setOs(null);

    // Act and Assert
    assertTrue(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code netware}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsNetware_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("netware");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code os/2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsOs2_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("os/2");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code os/400}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsOs400_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("os/400");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code tandem}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsTandem_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("tandem");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code unix}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsUnix_thenReturnTrue() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("unix");
    execTask.setOs(null);

    // Act and Assert
    assertTrue(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code win9x}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsWin9x_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("win9x");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code windows}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsWindows_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("windows");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code winnt}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsWinnt_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("winnt");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} OsFamily is {@code z/os}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsFamilyIsZOs_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("z/os");
    execTask.setOs(null);

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} Os is {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskOsIsFoo_thenReturnFalse() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setOsFamily("mac");
    execTask.setOs("foo");

    // Act and Assert
    assertFalse(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTaskProjectIsProject_thenReturnTrue() {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());

    // Act and Assert
    assertTrue(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenExecTask_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new ExecTask()).isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenJavaLangObject_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Current OS is ", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertTrue(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenProjectAddBuildListenerAntClassLoader_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act and Assert
    assertTrue(execTask.isValidOs());
  }

  /**
   * Test {@link ExecTask#prepareExec()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} addConfiguredRedirector {@link RedirectorElement} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#prepareExec()}
   */
  @Test
  public void testPrepareExec_givenExecTaskAddConfiguredRedirectorRedirectorElement() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.addConfiguredRedirector(new RedirectorElement());
    execTask.setProject(project);

    // Act
    Execute actualPrepareExecResult = execTask.prepareExec();

    // Assert
    Redirector redirector = execTask.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    File workingDirectory = actualPrepareExecResult.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualPrepareExecResult.getCommandline());
    assertNull(actualPrepareExecResult.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualPrepareExecResult.isFailure());
    assertEquals(Execute.INVALID, actualPrepareExecResult.getExitValue());
  }

  /**
   * Test {@link ExecTask#prepareExec()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#prepareExec()}
   */
  @Test
  public void testPrepareExec_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Users", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act
    Execute actualPrepareExecResult = execTask.prepareExec();

    // Assert
    Redirector redirector = execTask.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    File workingDirectory = actualPrepareExecResult.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualPrepareExecResult.getCommandline());
    assertNull(actualPrepareExecResult.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualPrepareExecResult.isFailure());
    assertEquals(Execute.INVALID, actualPrepareExecResult.getExitValue());
  }

  /**
   * Test {@link ExecTask#prepareExec()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#prepareExec()}
   */
  @Test
  public void testPrepareExec_givenProjectAddBuildListenerDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act
    Execute actualPrepareExecResult = execTask.prepareExec();

    // Assert
    Redirector redirector = execTask.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    File workingDirectory = actualPrepareExecResult.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualPrepareExecResult.getCommandline());
    assertNull(actualPrepareExecResult.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualPrepareExecResult.isFailure());
    assertEquals(Execute.INVALID, actualPrepareExecResult.getExitValue());
  }

  /**
   * Test {@link ExecTask#prepareExec()}.
   * <ul>
   *   <li>Then return WorkingDirectory Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#prepareExec()}
   */
  @Test
  public void testPrepareExec_thenReturnWorkingDirectoryNameIsApacheAnt11015() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());

    // Act
    Execute actualPrepareExecResult = execTask.prepareExec();

    // Assert
    Redirector redirector = execTask.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    File workingDirectory = actualPrepareExecResult.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualPrepareExecResult.getCommandline());
    assertNull(actualPrepareExecResult.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualPrepareExecResult.isFailure());
    assertEquals(Execute.INVALID, actualPrepareExecResult.getExitValue());
  }

  /**
   * Test {@link ExecTask#prepareExec()}.
   * <ul>
   *   <li>Then return WorkingDirectory Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#prepareExec()}
   */
  @Test
  public void testPrepareExec_thenReturnWorkingDirectoryNameIsApacheAnt110152() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);

    // Act
    Execute actualPrepareExecResult = execTask.prepareExec();

    // Assert
    Redirector redirector = execTask.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    File workingDirectory = actualPrepareExecResult.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualPrepareExecResult.getCommandline());
    assertNull(actualPrepareExecResult.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualPrepareExecResult.isFailure());
    assertEquals(Execute.INVALID, actualPrepareExecResult.getExitValue());
  }

  /**
   * Test {@link ExecTask#prepareExec()}.
   * <ul>
   *   <li>Then return WorkingDirectory Name is {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#prepareExec()}
   */
  @Test
  public void testPrepareExec_thenReturnWorkingDirectoryNameIsNullFile() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    Execute actualPrepareExecResult = execTask.prepareExec();

    // Assert
    Redirector redirector = execTask.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    File workingDirectory = actualPrepareExecResult.getWorkingDirectory();
    assertEquals("NULL_FILE", workingDirectory.getName());
    assertNull(actualPrepareExecResult.getCommandline());
    assertNull(actualPrepareExecResult.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualPrepareExecResult.isFailure());
    assertEquals(Execute.INVALID, actualPrepareExecResult.getExitValue());
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("Discarding output"));

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()} DiscardError is {@code true}.</li>
   *   <li>Then Out return {@link LogOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_givenExecTaskDiscardErrorIsTrue_thenOutReturnLogOutputStream() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setDiscardError(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    OutputStream out = ((PumpStreamHandler) actualCreateHandlerResult).getOut();
    assertTrue(out instanceof LogOutputStream);
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof NullOutputStream);
    assertEquals(2, ((LogOutputStream) out).getMessageLevel());
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Given {@link ExecTask#ExecTask()}.</li>
   *   <li>Then Err return {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_givenExecTask_thenErrReturnLineOrientedOutputStreamRedirector() throws BuildException {
    // Arrange and Act
    ExecuteStreamHandler actualCreateHandlerResult = (new ExecTask()).createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_givenProjectAddBuildListenerDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link Recorder} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_givenProjectAddBuildListenerRecorder() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new Recorder());

    ExecTask execTask = new ExecTask();
    execTask.setProject(project);
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Then Err return {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_thenErrReturnLineOrientedOutputStreamRedirector() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createHandler()}.
   * <ul>
   *   <li>Then Err return {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecTask#createHandler()}
   */
  @Test
  public void testCreateHandler_thenErrReturnLineOrientedOutputStreamRedirector2() throws BuildException {
    // Arrange
    ExecTask execTask = new ExecTask();
    execTask.setProject(new Project());
    execTask.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = execTask.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecTask#createWatchdog()}.
   * <p>
   * Method under test: {@link ExecTask#createWatchdog()}
   */
  @Test
  public void testCreateWatchdog() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new ExecTask()).createWatchdog());
  }
}

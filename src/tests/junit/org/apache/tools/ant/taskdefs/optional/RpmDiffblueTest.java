package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.taskdefs.Execute;
import org.apache.tools.ant.taskdefs.ExecuteStreamHandler;
import org.apache.tools.ant.taskdefs.PumpStreamHandler;
import org.apache.tools.ant.taskdefs.optional.RpmTest.MyRpm;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class RpmDiffblueTest {
  /**
   * Test {@link Rpm#execute()}.
   * <p>
   * Method under test: {@link Rpm#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Rpm rpm = new Rpm();
    rpm.setRpmBuildCommand(null);
    rpm.setTopDir(null);
    rpm.setCleanBuildDir(false);
    rpm.setRemoveSpec(false);
    rpm.setRemoveSource(false);
    rpm.setError(null);
    rpm.setOutput(Paths.get(System.getProperty("java.io.tmpdir"), "PATH", "getProject").toFile());
    rpm.setQuiet(false);
    rpm.setFailOnError(false);
    rpm.setCommand(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> rpm.execute());
  }

  /**
   * Test {@link Rpm#execute()}.
   * <p>
   * Method under test: {@link Rpm#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Rpm rpm = new Rpm();
    rpm.setRpmBuildCommand(null);
    rpm.setTopDir(null);
    rpm.setCleanBuildDir(false);
    rpm.setRemoveSpec(false);
    rpm.setRemoveSource(false);
    rpm.setError(Paths.get(System.getProperty("java.io.tmpdir"), "PATH", "getProject").toFile());
    rpm.setOutput(null);
    rpm.setQuiet(false);
    rpm.setFailOnError(false);
    rpm.setCommand(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> rpm.execute());
  }

  /**
   * Test {@link Rpm#execute()}.
   * <ul>
   *   <li>Given {@link MyRpm} (default constructor) FailOnError is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#execute()}
   */
  @Test
  public void testExecute_givenMyRpmFailOnErrorIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    MyRpm myRpm = new MyRpm();
    myRpm.setFailOnError(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> myRpm.execute());
  }

  /**
   * Test {@link Rpm#setSpecFile(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#setSpecFile(String)}
   */
  @Test
  public void testSetSpecFile_whenEmptyString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Rpm()).setSpecFile(""));
  }

  /**
   * Test {@link Rpm#setSpecFile(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#setSpecFile(String)}
   */
  @Test
  public void testSetSpecFile_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Rpm()).setSpecFile(null));
  }

  /**
   * Test {@link Rpm#guessRpmBuildCommand()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   *   <li>Then return {@code rpm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#guessRpmBuildCommand()}
   */
  @Test
  public void testGuessRpmBuildCommand_givenByteArrayOutputStreamWithOne_thenReturnRpm() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));

    Project project = new Project();
    project.addBuildListener(listener);

    Rpm rpm = new Rpm();
    rpm.setProject(project);

    // Act and Assert
    assertEquals("rpm", rpm.guessRpmBuildCommand());
  }

  /**
   * Test {@link Rpm#guessRpmBuildCommand()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code rpm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#guessRpmBuildCommand()}
   */
  @Test
  public void testGuessRpmBuildCommand_givenJavaLangObject_thenReturnRpm() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("getProject", typeClass);
    project.addBuildListener(new AntClassLoader());

    Rpm rpm = new Rpm();
    rpm.setProject(project);

    // Act and Assert
    assertEquals("rpm", rpm.guessRpmBuildCommand());
  }

  /**
   * Test {@link Rpm#guessRpmBuildCommand()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#guessRpmBuildCommand()}
   */
  @Test
  public void testGuessRpmBuildCommand_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rpm rpm = new Rpm();
    rpm.setProject(project);

    // Act and Assert
    assertEquals("rpm", rpm.guessRpmBuildCommand());
  }

  /**
   * Test {@link Rpm#guessRpmBuildCommand()}.
   * <ul>
   *   <li>Given {@link Rpm} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code rpm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#guessRpmBuildCommand()}
   */
  @Test
  public void testGuessRpmBuildCommand_givenRpmProjectIsProject_thenReturnRpm() {
    // Arrange
    Rpm rpm = new Rpm();
    rpm.setProject(new Project());

    // Act and Assert
    assertEquals("rpm", rpm.guessRpmBuildCommand());
  }

  /**
   * Test {@link Rpm#guessRpmBuildCommand()}.
   * <ul>
   *   <li>Given {@link Rpm} (default constructor).</li>
   *   <li>Then return {@code rpm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#guessRpmBuildCommand()}
   */
  @Test
  public void testGuessRpmBuildCommand_givenRpm_thenReturnRpm() {
    // Arrange, Act and Assert
    assertEquals("rpm", (new Rpm()).guessRpmBuildCommand());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute() {
    // Arrange
    Rpm rpm = new Rpm();
    rpm.setTopDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Commandline toExecute = new Commandline("To Process");

    // Act
    Execute actualExecute = rpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertEquals("test.txt", actualExecute.getWorkingDirectory().getName());
    assertArrayEquals(new String[]{"To", "Process"}, actualExecute.getCommandline());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    project.addBuildListener(new AntClassLoader());

    Rpm rpm = new Rpm();
    rpm.setProject(project);
    Commandline toExecute = new Commandline("To Process");

    // Act
    Execute actualExecute = rpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertEquals("test.txt", actualExecute.getWorkingDirectory().getName());
    assertArrayEquals(new String[]{"To", "Process"}, actualExecute.getCommandline());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("windows", typeClass);
    project.addBuildListener(new AntClassLoader());

    Rpm rpm = new Rpm();
    rpm.setProject(project);
    Commandline toExecute = new Commandline("To Process");

    // Act
    Execute actualExecute = rpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertEquals("apache-ant-1.10.15", actualExecute.getWorkingDirectory().getName());
    assertArrayEquals(new String[]{"To", "Process"}, actualExecute.getCommandline());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <ul>
   *   <li>Given {@link MyRpm} (default constructor).</li>
   *   <li>Then return Commandline is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute_givenMyRpm_thenReturnCommandlineIsNull() {
    // Arrange
    MyRpm myRpm = new MyRpm();
    Commandline toExecute = new Commandline("To Process");

    // Act
    Execute actualExecute = myRpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertNull(actualExecute.getCommandline());
    assertNull(actualExecute.getEnvironment());
    assertTrue(actualExecute.getWorkingDirectory().isAbsolute());
    assertTrue(actualExecute.isFailure());
    assertEquals(Integer.MAX_VALUE, actualExecute.getExitValue());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <ul>
   *   <li>Then return WorkingDirectory Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute_thenReturnWorkingDirectoryNameIsApacheAnt11015() {
    // Arrange
    Rpm rpm = new Rpm();
    rpm.setProject(new Project());
    Commandline toExecute = new Commandline("To Process");

    // Act
    Execute actualExecute = rpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertEquals("apache-ant-1.10.15", actualExecute.getWorkingDirectory().getName());
    assertArrayEquals(new String[]{"To", "Process"}, actualExecute.getCommandline());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <ul>
   *   <li>Then return WorkingDirectory Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute_thenReturnWorkingDirectoryNameIsApacheAnt110152() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Rpm rpm = new Rpm();
    rpm.setProject(project);
    Commandline toExecute = new Commandline("To Process");

    // Act
    Execute actualExecute = rpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertEquals("apache-ant-1.10.15", actualExecute.getWorkingDirectory().getName());
    assertArrayEquals(new String[]{"To", "Process"}, actualExecute.getCommandline());
  }

  /**
   * Test {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with toProcess is {@code null}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rpm#getExecute(Commandline, ExecuteStreamHandler)}
   */
  @Test
  public void testGetExecute_whenCommandlineWithToProcessIsNull_thenReturnArrayLengthIsZero() {
    // Arrange
    Rpm rpm = new Rpm();
    rpm.setProject(new Project());
    Commandline toExecute = new Commandline(null);

    // Act
    Execute actualExecute = rpm.getExecute(toExecute, new PumpStreamHandler());

    // Assert
    assertNull(actualExecute.getEnvironment());
    assertEquals(0, actualExecute.getCommandline().length);
    assertTrue(actualExecute.getWorkingDirectory().isAbsolute());
    assertTrue(actualExecute.isFailure());
    assertEquals(Integer.MAX_VALUE, actualExecute.getExitValue());
  }

  /**
   * Test new {@link Rpm} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Rpm}
   */
  @Test
  public void testNewRpm() {
    // Arrange and Act
    Rpm actualRpm = new Rpm();

    // Assert
    Location location = actualRpm.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRpm.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualRpm.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualRpm.getTaskName());
    assertNull(actualRpm.getTaskType());
    assertNull(actualRpm.getProject());
    assertNull(actualRpm.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualRpm, runtimeConfigurableWrapper.getProxy());
  }
}

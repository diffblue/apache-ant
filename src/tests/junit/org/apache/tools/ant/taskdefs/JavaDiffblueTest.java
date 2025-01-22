package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.Definer.OnError;
import org.apache.tools.ant.types.Assertions;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.CommandlineJava;
import org.apache.tools.ant.types.CommandlineJava.SysProperties;
import org.apache.tools.ant.types.Environment;
import org.apache.tools.ant.types.Environment.Variable;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Permissions;
import org.apache.tools.ant.types.PropertySet;
import org.apache.tools.ant.types.RedirectorElement;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.util.LineOrientedOutputStreamRedirector;
import org.apache.tools.ant.util.NullOutputStream;
import org.junit.Test;

public class JavaDiffblueTest {
  /**
   * Test {@link Java#Java()}.
   * <p>
   * Method under test: {@link Java#Java()}
   */
  @Test
  public void testNewJava() {
    // Arrange and Act
    Java actualJava = new Java();

    // Assert
    assertNull(actualJava.getDescription());
    assertNull(actualJava.getTaskName());
    assertNull(actualJava.getTaskType());
    assertNull(actualJava.getProject());
    assertNull(actualJava.getOwningTarget());
    assertNull(actualJava.redirectorElement);
  }

  /**
   * Test {@link Java#Java(Task)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#Java(Task)}
   */
  @Test
  public void testNewJava_whenTaskAdapter_thenReturnDescriptionIsNull() {
    // Arrange and Act
    Java actualJava = new Java(new TaskAdapter());

    // Assert
    assertNull(actualJava.getDescription());
    assertNull(actualJava.getTaskName());
    assertNull(actualJava.getTaskType());
    assertNull(actualJava.getProject());
    assertNull(actualJava.getOwningTarget());
    assertNull(actualJava.redirectorElement);
  }

  /**
   * Test {@link Java#execute()}.
   * <p>
   * Method under test: {@link Java#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Java()).execute());
  }

  /**
   * Test {@link Java#executeJava()}.
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet sysp = new PropertySet();
    sysp.appendName("key and value must be specified for environment variables.");

    Java java = new Java();
    java.addSyspropertyset(sysp);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("java.lang.NullPointerException", typeClass);
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava(new CommandlineJava());

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava3() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(OnError.POLICY_IGNORE, typeClass);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.createClasspath(p);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava4() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    PropertySet sysp = new PropertySet();
    sysp.appendName("key and value must be specified for environment variables.");

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSyspropertyset(sysp);
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava5() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setFork(true);
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.setMaxmemory("key and value must be specified for environment variables.");
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link Java#Java()} DiscardOutput is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenJavaDiscardOutputIsTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setDiscardOutput(true);
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>When {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenJava_whenCommandlineJava() {
    // Arrange
    Java java = new Java();

    // Act
    int actualExecuteJavaResult = java.executeJava(new CommandlineJava());

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenProject() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.createClasspath(new Project());

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenPropertySet() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSyspropertyset(new PropertySet());
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenPropertySetAddCutDirsMapper() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    PropertySet sysp = new PropertySet();
    sysp.add(new CutDirsMapper());
    sysp.appendName("key and value must be specified for environment variables.");

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSyspropertyset(sysp);
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Negate is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenPropertySetNegateIsTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    PropertySet sysp = new PropertySet();
    sysp.setNegate(true);
    sysp.add(new CutDirsMapper());
    sysp.appendName("key and value must be specified for environment variables.");

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSyspropertyset(sysp);
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenPropertySetProjectIsProject() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    PropertySet sysp = new PropertySet();
    sysp.setProject(new Project());
    sysp.appendName("key and value must be specified for environment variables.");

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSyspropertyset(sysp);
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Given {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_givenVariable() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} ErrorStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_thenJavaRedirectorErrorStreamIsNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setFork(true);
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} OutputStream {@link LogOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_thenJavaRedirectorOutputStreamLogOutputStream() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setDiscardError(true);
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    OutputStream outputStream = redirector.getOutputStream();
    assertTrue(outputStream instanceof LogOutputStream);
    assertTrue(redirector.getErrorStream() instanceof NullOutputStream);
    assertEquals(-1, actualExecuteJavaResult);
    assertEquals(2, ((LogOutputStream) outputStream).getMessageLevel());
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>When {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_whenCommandlineJava() {
    // Arrange
    Java java = new Java();
    java.setProject(new Project());

    // Act
    int actualExecuteJavaResult = java.executeJava(new CommandlineJava());

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>When {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_whenCommandlineJava2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava(new CommandlineJava());

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>When {@link CommandlineJava} (default constructor) addSysproperty {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_whenCommandlineJavaAddSyspropertyNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSysproperty(null);
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava(CommandlineJava)} with {@code CommandlineJava}.
   * <ul>
   *   <li>When {@link CommandlineJava} (default constructor) addSyspropertyset {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava(CommandlineJava)}
   */
  @Test
  public void testExecuteJavaWithCommandlineJava_whenCommandlineJavaAddSyspropertysetNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    CommandlineJava commandLine = new CommandlineJava();
    commandLine.addSyspropertyset(null);
    commandLine.addSysproperty(new Variable());
    commandLine.createClasspath(null);

    // Act
    int actualExecuteJavaResult = java.executeJava(commandLine);

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJava() throws BuildException {
    // Arrange
    Java java = new Java();

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} addSysproperty {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaAddSyspropertyNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.addSysproperty(null);
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} addSysproperty {@code null}.</li>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} ErrorStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaAddSyspropertyNull_thenJavaRedirectorErrorStreamIsNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setFork(true);
    java.addSysproperty(null);
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} addSysproperty {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaAddSyspropertyVariable() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} addSyspropertyset {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaAddSyspropertysetNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.addSyspropertyset(null);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} addSyspropertyset {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaAddSyspropertysetPropertySet() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.addSyspropertyset(new PropertySet());
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} DiscardOutput is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaDiscardOutputIsTrue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setDiscardOutput(true);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link Java#Java()} Fork is {@code true}.</li>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} ErrorStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaForkIsTrue_thenJavaRedirectorErrorStreamIsNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setFork(true);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("java.lang.NullPointerException", typeClass);
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPropertySetAddCutDirsMapper() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet sysp = new PropertySet();
    sysp.add(new CutDirsMapper());
    sysp.appendName("key and value must be specified for environment variables.");

    Java java = new Java();
    java.addSyspropertyset(sysp);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPropertySetAddFilterMapper() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet sysp = new PropertySet();
    sysp.setNegate(true);
    sysp.add(new FilterMapper());
    sysp.appendName("key and value must be specified for environment variables.");

    Java java = new Java();
    java.addSyspropertyset(sysp);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Negate is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPropertySetNegateIsTrue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet sysp = new PropertySet();
    sysp.setNegate(true);
    sysp.add(new CutDirsMapper());
    sysp.appendName("key and value must be specified for environment variables.");

    Java java = new Java();
    java.addSyspropertyset(sysp);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPropertySetProjectIsProject() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet sysp = new PropertySet();
    sysp.setProject(new Project());
    sysp.appendName("key and value must be specified for environment variables.");

    Java java = new Java();
    java.addSyspropertyset(sysp);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} ErrorStream {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_thenJavaRedirectorErrorStreamLineOrientedOutputStreamRedirector() throws BuildException {
    // Arrange
    Java java = new Java();
    java.setProject(new Project());

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} ErrorStream {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_thenJavaRedirectorErrorStreamLineOrientedOutputStreamRedirector2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertEquals(-1, actualExecuteJavaResult);
  }

  /**
   * Test {@link Java#executeJava()}.
   * <ul>
   *   <li>Then {@link Java#Java()} {@link Java#redirector} OutputStream {@link LogOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#executeJava()}
   */
  @Test
  public void testExecuteJava_thenJavaRedirectorOutputStreamLogOutputStream() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Java java = new Java();
    java.setDiscardError(true);
    java.addSysproperty(new Variable());
    java.setProject(project);

    // Act
    int actualExecuteJavaResult = java.executeJava();

    // Assert
    Redirector redirector = java.redirector;
    OutputStream outputStream = redirector.getOutputStream();
    assertTrue(outputStream instanceof LogOutputStream);
    assertTrue(redirector.getErrorStream() instanceof NullOutputStream);
    assertEquals(-1, actualExecuteJavaResult);
    assertEquals(2, ((LogOutputStream) outputStream).getMessageLevel());
  }

  /**
   * Test {@link Java#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Java#Java()} CommandLine Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenJavaCommandLineClasspathDescriptionIsNull() {
    // Arrange
    Java java = new Java();

    // Act
    java.setClasspath(null);

    // Assert
    Path classpath = java.getCommandLine().getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Java#checkConfiguration()}.
   * <p>
   * Method under test: {@link Java#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Java()).checkConfiguration());
  }

  /**
   * Test {@link Java#createClasspath()}.
   * <ul>
   *   <li>Given {@link Java#Java()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJavaProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Java java = new Java();
    Project project = new Project();
    java.setProject(project);

    // Act and Assert
    assertSame(project, java.createClasspath().getProject());
  }

  /**
   * Test {@link Java#createClasspath()}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJava_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new Java()).createClasspath();

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
   * Test {@link Java#createBootclasspath()}.
   * <ul>
   *   <li>Given {@link Java#Java()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_givenJavaProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Java java = new Java();
    Project project = new Project();
    java.setProject(project);

    // Act and Assert
    assertSame(project, java.createBootclasspath().getProject());
  }

  /**
   * Test {@link Java#createBootclasspath()}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_givenJava_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateBootclasspathResult = (new Java()).createBootclasspath();

    // Assert
    Location location = actualCreateBootclasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateBootclasspathResult.getDescription());
    assertNull(actualCreateBootclasspathResult.getProject());
    assertNull(actualCreateBootclasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateBootclasspathResult.size());
    assertFalse(actualCreateBootclasspathResult.isReference());
    assertTrue(actualCreateBootclasspathResult.isEmpty());
  }

  /**
   * Test {@link Java#createModulepath()}.
   * <ul>
   *   <li>Given {@link Java#Java()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createModulepath()}
   */
  @Test
  public void testCreateModulepath_givenJavaProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Java java = new Java();
    Project project = new Project();
    java.setProject(project);

    // Act and Assert
    assertSame(project, java.createModulepath().getProject());
  }

  /**
   * Test {@link Java#createModulepath()}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createModulepath()}
   */
  @Test
  public void testCreateModulepath_givenJava_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateModulepathResult = (new Java()).createModulepath();

    // Assert
    Location location = actualCreateModulepathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateModulepathResult.getDescription());
    assertNull(actualCreateModulepathResult.getProject());
    assertNull(actualCreateModulepathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateModulepathResult.size());
    assertFalse(actualCreateModulepathResult.isReference());
    assertTrue(actualCreateModulepathResult.isEmpty());
  }

  /**
   * Test {@link Java#createUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link Java#Java()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_givenJavaProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Java java = new Java();
    Project project = new Project();
    java.setProject(project);

    // Act and Assert
    assertSame(project, java.createUpgrademodulepath().getProject());
  }

  /**
   * Test {@link Java#createUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_givenJava_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateUpgrademodulepathResult = (new Java()).createUpgrademodulepath();

    // Assert
    Location location = actualCreateUpgrademodulepathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUpgrademodulepathResult.getDescription());
    assertNull(actualCreateUpgrademodulepathResult.getProject());
    assertNull(actualCreateUpgrademodulepathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateUpgrademodulepathResult.size());
    assertFalse(actualCreateUpgrademodulepathResult.isReference());
    assertTrue(actualCreateUpgrademodulepathResult.isEmpty());
  }

  /**
   * Test {@link Java#createPermissions()}.
   * <p>
   * Method under test: {@link Java#createPermissions()}
   */
  @Test
  public void testCreatePermissions() {
    // Arrange and Act
    Permissions actualCreatePermissionsResult = (new Java()).createPermissions();

    // Assert
    Location location = actualCreatePermissionsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePermissionsResult.getDescription());
    assertNull(actualCreatePermissionsResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Java#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then {@link Java#Java()} CommandLine Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenJava_thenJavaCommandLineClasspathProjectIsNull() {
    // Arrange
    Java java = new Java();

    // Act
    java.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = java.getCommandLine().getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link Java#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link Java#Java()} CommandLine Classpath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenJavaCommandLineClasspathProjectIsProject() {
    // Arrange
    Java java = new Java();
    Project project = new Project();
    java.setProject(project);

    // Act
    java.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = java.getCommandLine().getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link Java#setClassname(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Java#Java()} CommandLine JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenEmptyString_thenJavaCommandLineJavaCommandSizeIsZero() throws BuildException {
    // Arrange
    Java java = new Java();

    // Act
    java.setClassname("");

    // Assert that nothing has changed
    CommandlineJava commandLine = java.getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, commandLine.size());
    assertEquals(1, commandLine.getCommandline().length);
  }

  /**
   * Test {@link Java#setClassname(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then {@link Java#Java()} CommandLine JavaCommand Executable is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenFoo_thenJavaCommandLineJavaCommandExecutableIsFoo() throws BuildException {
    // Arrange
    Java java = new Java();

    // Act
    java.setClassname("foo");

    // Assert
    CommandlineJava commandLine = java.getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals("foo", javaCommand.getExecutable());
    assertEquals("foo", commandLine.getClassname());
    String[] commandline = commandLine.getCommandline();
    assertEquals("foo", commandline[1]);
    assertEquals(1, javaCommand.size());
    assertEquals(2, commandLine.size());
    assertEquals(2, commandline.length);
    assertArrayEquals(new String[]{"foo"}, javaCommand.getCommandline());
  }

  /**
   * Test {@link Java#setClassname(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Java#Java()} CommandLine JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenNull_thenJavaCommandLineJavaCommandSizeIsZero() throws BuildException {
    // Arrange
    Java java = new Java();

    // Act
    java.setClassname(null);

    // Assert that nothing has changed
    CommandlineJava commandLine = java.getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, commandLine.size());
    assertEquals(1, commandLine.getCommandline().length);
  }

  /**
   * Test {@link Java#setArgs(String)}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>When empty string.</li>
   *   <li>Then {@link Java#Java()} CommandLine JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setArgs(String)}
   */
  @Test
  public void testSetArgs_givenJava_whenEmptyString_thenJavaCommandLineJavaCommandSizeIsZero() {
    // Arrange
    Java java = new Java();

    // Act
    java.setArgs("");

    // Assert
    CommandlineJava commandLine = java.getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    assertEquals(0, iteratorResult.next().getParts().length);
    assertEquals(1, commandLine.size());
    assertEquals(1, commandLine.getCommandline().length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link Java#setArgs(String)}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>When space.</li>
   *   <li>Then {@link Java#Java()} CommandLine JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setArgs(String)}
   */
  @Test
  public void testSetArgs_givenJava_whenSpace_thenJavaCommandLineJavaCommandSizeIsZero() {
    // Arrange
    Java java = new Java();

    // Act
    java.setArgs(" ");

    // Assert
    CommandlineJava commandLine = java.getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    assertEquals(0, iteratorResult.next().getParts().length);
    assertEquals(1, commandLine.size());
    assertEquals(1, commandLine.getCommandline().length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link Java#setArgs(String)}.
   * <ul>
   *   <li>Then seventh element is {@code Please}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setArgs(String)}
   */
  @Test
  public void testSetArgs_thenSeventhElementIsPlease() {
    // Arrange
    Java java = new Java();

    // Act
    java.setArgs("The args attribute is deprecated. Please use nested arg elements.");

    // Assert
    CommandlineJava commandLine = java.getCommandLine();
    String[] commandline = commandLine.getCommandline();
    assertEquals("Please", commandline[6]);
    assertEquals("The", commandline[1]);
    assertEquals("arg", commandline[9]);
    assertEquals("args", commandline[2]);
    assertEquals("attribute", commandline[3]);
    assertEquals("deprecated.", commandline[5]);
    assertEquals("elements.", commandline[10]);
    assertEquals("is", commandline[4]);
    assertEquals("nested", commandline[8]);
    assertEquals("use", commandline[7]);
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals(10, javaCommand.size());
    assertEquals(10, javaCommand.getArguments().length);
    assertEquals(10, javaCommand.getCommandline().length);
    assertEquals(11, commandLine.size());
    assertEquals(11, commandline.length);
  }

  /**
   * Test {@link Java#setArgs(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Java#Java()} CommandLine JavaCommand iterator next Parts is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#setArgs(String)}
   */
  @Test
  public void testSetArgs_whenNull_thenJavaCommandLineJavaCommandIteratorNextPartsIsNull() {
    // Arrange
    Java java = new Java();

    // Act
    java.setArgs(null);

    // Assert
    CommandlineJava commandLine = java.getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    assertNull(iteratorResult.next().getParts());
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, commandLine.size());
    assertEquals(1, commandLine.getCommandline().length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link Java#addSysproperty(Variable)}.
   * <p>
   * Method under test: {@link Java#addSysproperty(Variable)}
   */
  @Test
  public void testAddSysproperty() {
    // Arrange
    Java java = new Java();
    Variable sysp = new Variable();

    // Act
    java.addSysproperty(sysp);

    // Assert
    SysProperties sysProperties = java.getSysProperties();
    Vector<Variable> variablesVector = sysProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, sysProperties.size());
    assertEquals(2, java.getCommandLine().size());
    assertSame(sysp, variablesVector.get(0));
  }

  /**
   * Test {@link Java#addAssertions(Assertions)}.
   * <ul>
   *   <li>Given {@link Java#Java()} addAssertions {@link Assertions} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#addAssertions(Assertions)}
   */
  @Test
  public void testAddAssertions_givenJavaAddAssertionsAssertions_thenThrowBuildException() {
    // Arrange
    Java java = new Java();
    java.addAssertions(new Assertions());

    // Act and Assert
    assertThrows(BuildException.class, () -> java.addAssertions(new Assertions()));
  }

  /**
   * Test {@link Java#addAssertions(Assertions)}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then {@link Java#Java()} CommandLine Assertions is {@link Assertions} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#addAssertions(Assertions)}
   */
  @Test
  public void testAddAssertions_givenJava_thenJavaCommandLineAssertionsIsAssertions() {
    // Arrange
    Java java = new Java();
    Assertions asserts = new Assertions();

    // Act
    java.addAssertions(asserts);

    // Assert
    assertSame(asserts, java.getCommandLine().getAssertions());
  }

  /**
   * Test {@link Java#addConfiguredRedirector(RedirectorElement)}.
   * <ul>
   *   <li>Given {@link Java#Java()}.</li>
   *   <li>Then {@link Java#Java()} {@link Java#redirectorElement} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#addConfiguredRedirector(RedirectorElement)}
   */
  @Test
  public void testAddConfiguredRedirector_givenJava_thenJavaRedirectorElementDescriptionIsNull() {
    // Arrange
    Java java = new Java();

    // Act
    java.addConfiguredRedirector(new RedirectorElement());

    // Assert
    RedirectorElement redirectorElement = java.redirectorElement;
    assertNull(redirectorElement.getDescription());
    assertNull(redirectorElement.getProject());
    assertNull(redirectorElement.getRefid());
    assertFalse(redirectorElement.isReference());
  }

  /**
   * Test {@link Java#addConfiguredRedirector(RedirectorElement)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#addConfiguredRedirector(RedirectorElement)}
   */
  @Test
  public void testAddConfiguredRedirector_thenThrowBuildException() {
    // Arrange
    Java java = new Java();
    java.addConfiguredRedirector(new RedirectorElement());

    // Act and Assert
    assertThrows(BuildException.class, () -> java.addConfiguredRedirector(new RedirectorElement()));
  }

  /**
   * Test {@link Java#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Java#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    Java java = new Java();
    java.setProject(project);

    // Act and Assert
    assertEquals(3, java.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, java.getProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Java#createWatchdog()}.
   * <p>
   * Method under test: {@link Java#createWatchdog()}
   */
  @Test
  public void testCreateWatchdog() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new Java()).createWatchdog());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Java#setDir(File)}
   *   <li>{@link Java#setFork(boolean)}
   *   <li>{@link Java#setNewenvironment(boolean)}
   *   <li>{@link Java#setSpawn(boolean)}
   *   <li>{@link Java#getCommandLine()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange
    Java java = new Java();

    // Act
    java.setDir(Copy.NULL_FILE_PLACEHOLDER);
    java.setFork(true);
    java.setNewenvironment(true);
    java.setSpawn(true);
    CommandlineJava actualCommandLine = java.getCommandLine();

    // Assert
    assertEquals("17", actualCommandLine.getVmversion());
    SysProperties systemProperties = actualCommandLine.getSystemProperties();
    assertNull(systemProperties.getVariables());
    Commandline javaCommand = actualCommandLine.getJavaCommand();
    assertNull(javaCommand.getExecutable());
    assertNull(actualCommandLine.getClassname());
    assertNull(actualCommandLine.getJar());
    assertNull(actualCommandLine.getModule());
    assertNull(actualCommandLine.getSourceFile());
    assertNull(actualCommandLine.getAssertions());
    assertNull(actualCommandLine.getBootclasspath());
    assertNull(actualCommandLine.getClasspath());
    assertNull(actualCommandLine.getModulepath());
    assertNull(actualCommandLine.getUpgrademodulepath());
    assertEquals(0, javaCommand.size());
    assertEquals(0, systemProperties.size());
    assertEquals(0, javaCommand.getArguments().length);
    Commandline vmCommand = actualCommandLine.getVmCommand();
    assertEquals(0, vmCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, vmCommand.size());
    assertEquals(1, actualCommandLine.size());
    assertFalse(javaCommand.iterator().hasNext());
    assertFalse(vmCommand.iterator().hasNext());
    String expectedExecutable = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    assertEquals(expectedExecutable, vmCommand.getExecutable());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        vmCommand.getCommandline());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandLine.getCommandline());
  }

  /**
   * Test {@link Java#getSysProperties()}.
   * <p>
   * Method under test: {@link Java#getSysProperties()}
   */
  @Test
  public void testGetSysProperties() throws BuildException {
    // Arrange and Act
    SysProperties actualSysProperties = (new Java()).getSysProperties();

    // Assert
    assertNull(actualSysProperties.getVariables());
    assertEquals(0, actualSysProperties.size());
    assertTrue(actualSysProperties.getVariablesVector().isEmpty());
  }
}

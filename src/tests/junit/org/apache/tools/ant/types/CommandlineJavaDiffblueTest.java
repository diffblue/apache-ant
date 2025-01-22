package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.CommandlineJava.SysProperties;
import org.apache.tools.ant.types.Environment.Variable;
import org.junit.Test;

public class CommandlineJavaDiffblueTest {
  /**
   * Test new {@link CommandlineJava} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CommandlineJava}
   */
  @Test
  public void testNewCommandlineJava() {
    // Arrange and Act
    CommandlineJava actualCommandlineJava = new CommandlineJava();

    // Assert
    assertEquals("17", actualCommandlineJava.getVmversion());
    assertNull(actualCommandlineJava.getClassname());
    assertNull(actualCommandlineJava.getJar());
    assertNull(actualCommandlineJava.getModule());
    assertNull(actualCommandlineJava.getSourceFile());
    assertNull(actualCommandlineJava.getAssertions());
    assertNull(actualCommandlineJava.getBootclasspath());
    assertNull(actualCommandlineJava.getClasspath());
    assertNull(actualCommandlineJava.getModulepath());
    assertNull(actualCommandlineJava.getUpgrademodulepath());
    assertEquals(1, actualCommandlineJava.size());
    assertEquals(1, actualCommandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#addSysproperty(Variable)}.
   * <p>
   * Method under test: {@link CommandlineJava#addSysproperty(Variable)}
   */
  @Test
  public void testAddSysproperty() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    Variable sysp = new Variable();

    // Act
    commandlineJava.addSysproperty(sysp);

    // Assert
    SysProperties systemProperties = commandlineJava.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(2, commandlineJava.size());
    assertSame(sysp, variablesVector.get(0));
  }

  /**
   * Test {@link CommandlineJava#setVm(String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setVm(String)}
   */
  @Test
  public void testSetVm_whenEmptyString() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setVm("");

    // Assert that nothing has changed
    String expectedExecutable = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    Commandline actualVMCommand = commandlineJava.getActualVMCommand();
    assertEquals(expectedExecutable, actualVMCommand.getExecutable());
    String expectedExecutable2 = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    Commandline vmCommand = commandlineJava.getVmCommand();
    assertEquals(expectedExecutable2, vmCommand.getExecutable());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualVMCommand.getCommandline());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        vmCommand.getCommandline());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        commandlineJava.getCommandline());
  }

  /**
   * Test {@link CommandlineJava#setVm(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setVm(String)}
   */
  @Test
  public void testSetVm_whenNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setVm(null);

    // Assert that nothing has changed
    String expectedExecutable = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    Commandline actualVMCommand = commandlineJava.getActualVMCommand();
    assertEquals(expectedExecutable, actualVMCommand.getExecutable());
    String expectedExecutable2 = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    Commandline vmCommand = commandlineJava.getVmCommand();
    assertEquals(expectedExecutable2, vmCommand.getExecutable());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualVMCommand.getCommandline());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        vmCommand.getCommandline());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        commandlineJava.getCommandline());
  }

  /**
   * Test {@link CommandlineJava#setVm(String)}.
   * <ul>
   *   <li>When {@code Vm}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) ActualVMCommand Executable is {@code Vm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setVm(String)}
   */
  @Test
  public void testSetVm_whenVm_thenCommandlineJavaActualVMCommandExecutableIsVm() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setVm("Vm");

    // Assert
    Commandline actualVMCommand = commandlineJava.getActualVMCommand();
    assertEquals("Vm", actualVMCommand.getExecutable());
    Commandline vmCommand = commandlineJava.getVmCommand();
    assertEquals("Vm", vmCommand.getExecutable());
    assertArrayEquals(new String[]{"Vm"}, actualVMCommand.getCommandline());
    assertArrayEquals(new String[]{"Vm"}, vmCommand.getCommandline());
    assertArrayEquals(new String[]{"Vm"}, commandlineJava.getCommandline());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CommandlineJava#setAssertions(Assertions)}
   *   <li>{@link CommandlineJava#setCloneVm(boolean)}
   *   <li>{@link CommandlineJava#setMaxmemory(String)}
   *   <li>{@link CommandlineJava#setVmversion(String)}
   *   <li>{@link CommandlineJava#getAssertions()}
   *   <li>{@link CommandlineJava#getBootclasspath()}
   *   <li>{@link CommandlineJava#getClasspath()}
   *   <li>{@link CommandlineJava#getJavaCommand()}
   *   <li>{@link CommandlineJava#getModulepath()}
   *   <li>{@link CommandlineJava#getSystemProperties()}
   *   <li>{@link CommandlineJava#getUpgrademodulepath()}
   *   <li>{@link CommandlineJava#getVmversion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    Assertions assertions = new Assertions();

    // Act
    commandlineJava.setAssertions(assertions);
    commandlineJava.setCloneVm(true);
    commandlineJava.setMaxmemory("Max");
    commandlineJava.setVmversion("42");
    Assertions actualAssertions = commandlineJava.getAssertions();
    Path actualBootclasspath = commandlineJava.getBootclasspath();
    Path actualClasspath = commandlineJava.getClasspath();
    Commandline actualJavaCommand = commandlineJava.getJavaCommand();
    Path actualModulepath = commandlineJava.getModulepath();
    SysProperties actualSystemProperties = commandlineJava.getSystemProperties();
    Path actualUpgrademodulepath = commandlineJava.getUpgrademodulepath();

    // Assert
    assertEquals("42", commandlineJava.getVmversion());
    assertNull(actualSystemProperties.getVariables());
    assertNull(actualJavaCommand.getExecutable());
    assertNull(actualSystemProperties.sys);
    assertNull(actualBootclasspath);
    assertNull(actualClasspath);
    assertNull(actualModulepath);
    assertNull(actualUpgrademodulepath);
    assertEquals(0, actualJavaCommand.size());
    assertEquals(0, actualSystemProperties.size());
    assertEquals(0, actualJavaCommand.getArguments().length);
    assertEquals(0, actualJavaCommand.getCommandline().length);
    assertFalse(actualJavaCommand.iterator().hasNext());
    assertSame(assertions, actualAssertions);
  }

  /**
   * Test {@link CommandlineJava#setJar(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setJar(String)}
   */
  @Test
  public void testSetJar_whenEmptyString_thenCommandlineJavaJavaCommandExecutableIsNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setJar("");

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertNull(javaCommand.getExecutable());
    assertNull(commandlineJava.getJar());
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(2, commandlineJava.size());
    assertEquals(2, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#setJar(String)}.
   * <ul>
   *   <li>When {@code Jarpathname}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code Jarpathname}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setJar(String)}
   */
  @Test
  public void testSetJar_whenJarpathname_thenCommandlineJavaJavaCommandExecutableIsJarpathname() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setJar("Jarpathname");

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals("Jarpathname", javaCommand.getExecutable());
    assertEquals("Jarpathname", commandlineJava.getJar());
    String[] commandline = commandlineJava.getCommandline();
    assertEquals("Jarpathname", commandline[2]);
    assertEquals(1, javaCommand.size());
    assertEquals(3, commandlineJava.size());
    assertEquals(3, commandline.length);
    assertArrayEquals(new String[]{"Jarpathname"}, javaCommand.getCommandline());
  }

  /**
   * Test {@link CommandlineJava#setJar(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setJar(String)}
   */
  @Test
  public void testSetJar_whenNull_thenCommandlineJavaJavaCommandExecutableIsNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setJar(null);

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertNull(javaCommand.getExecutable());
    assertNull(commandlineJava.getJar());
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(2, commandlineJava.size());
    assertEquals(2, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#getJar()}.
   * <p>
   * Method under test: {@link CommandlineJava#getJar()}
   */
  @Test
  public void testGetJar() {
    // Arrange, Act and Assert
    assertNull((new CommandlineJava()).getJar());
  }

  /**
   * Test {@link CommandlineJava#setClassname(String)}.
   * <ul>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code Classname}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setClassname(String)}
   */
  @Test
  public void testSetClassname_thenCommandlineJavaJavaCommandExecutableIsClassname() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setClassname("Classname");

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals("Classname", javaCommand.getExecutable());
    assertEquals("Classname", commandlineJava.getClassname());
    String[] commandline = commandlineJava.getCommandline();
    assertEquals("Classname", commandline[1]);
    assertEquals(1, javaCommand.size());
    assertEquals(2, commandlineJava.size());
    assertEquals(2, commandline.length);
    assertArrayEquals(new String[]{"Classname"}, javaCommand.getCommandline());
  }

  /**
   * Test {@link CommandlineJava#setClassname(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setClassname(String)}
   */
  @Test
  public void testSetClassname_whenEmptyString_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setClassname("");

    // Assert that nothing has changed
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, commandlineJava.size());
    assertEquals(1, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#getClassname()}.
   * <p>
   * Method under test: {@link CommandlineJava#getClassname()}
   */
  @Test
  public void testGetClassname() {
    // Arrange, Act and Assert
    assertNull((new CommandlineJava()).getClassname());
  }

  /**
   * Test {@link CommandlineJava#setSourceFile(String)}.
   * <ul>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code Source File}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setSourceFile(String)}
   */
  @Test
  public void testSetSourceFile_thenCommandlineJavaJavaCommandExecutableIsSourceFile() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setSourceFile("Source File");

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals("Source File", javaCommand.getExecutable());
    assertEquals("Source File", commandlineJava.getSourceFile());
    String[] commandline = commandlineJava.getCommandline();
    assertEquals("Source File", commandline[1]);
    assertEquals(1, javaCommand.size());
    assertEquals(2, commandlineJava.size());
    assertEquals(2, commandline.length);
    assertArrayEquals(new String[]{"Source File"}, javaCommand.getCommandline());
  }

  /**
   * Test {@link CommandlineJava#setSourceFile(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setSourceFile(String)}
   */
  @Test
  public void testSetSourceFile_whenEmptyString_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setSourceFile("");

    // Assert that nothing has changed
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, commandlineJava.size());
    assertEquals(1, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#setSourceFile(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setSourceFile(String)}
   */
  @Test
  public void testSetSourceFile_whenNull_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setSourceFile(null);

    // Assert that nothing has changed
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, commandlineJava.size());
    assertEquals(1, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#getSourceFile()}.
   * <p>
   * Method under test: {@link CommandlineJava#getSourceFile()}
   */
  @Test
  public void testGetSourceFile() {
    // Arrange, Act and Assert
    assertNull((new CommandlineJava()).getSourceFile());
  }

  /**
   * Test {@link CommandlineJava#setModule(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setModule(String)}
   */
  @Test
  public void testSetModule_whenEmptyString_thenCommandlineJavaJavaCommandExecutableIsNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setModule("");

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertNull(javaCommand.getExecutable());
    assertNull(commandlineJava.getModule());
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(2, commandlineJava.size());
    assertEquals(2, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#setModule(String)}.
   * <ul>
   *   <li>When {@code Module}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code Module}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setModule(String)}
   */
  @Test
  public void testSetModule_whenModule_thenCommandlineJavaJavaCommandExecutableIsModule() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setModule("Module");

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertEquals("Module", javaCommand.getExecutable());
    assertEquals("Module", commandlineJava.getModule());
    String[] commandline = commandlineJava.getCommandline();
    assertEquals("Module", commandline[2]);
    assertEquals(1, javaCommand.size());
    assertEquals(3, commandlineJava.size());
    assertEquals(3, commandline.length);
    assertArrayEquals(new String[]{"Module"}, javaCommand.getCommandline());
  }

  /**
   * Test {@link CommandlineJava#setModule(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#setModule(String)}
   */
  @Test
  public void testSetModule_whenNull_thenCommandlineJavaJavaCommandExecutableIsNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();

    // Act
    commandlineJava.setModule(null);

    // Assert
    Commandline javaCommand = commandlineJava.getJavaCommand();
    assertNull(javaCommand.getExecutable());
    assertNull(commandlineJava.getModule());
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(2, commandlineJava.size());
    assertEquals(2, commandlineJava.getCommandline().length);
  }

  /**
   * Test {@link CommandlineJava#getModule()}.
   * <p>
   * Method under test: {@link CommandlineJava#getModule()}
   */
  @Test
  public void testGetModule() {
    // Arrange, Act and Assert
    assertNull((new CommandlineJava()).getModule());
  }

  /**
   * Test {@link CommandlineJava#createClasspath(Project)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createClasspath(Project)}
   */
  @Test
  public void testCreateClasspath_givenCommandlineJava_thenReturnProjectIsProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    Project p = new Project();

    // Act and Assert
    assertSame(p, commandlineJava.createClasspath(p).getProject());
  }

  /**
   * Test {@link CommandlineJava#createClasspath(Project)}.
   * <ul>
   *   <li>Then return DataTypeName is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createClasspath(Project)}
   */
  @Test
  public void testCreateClasspath_thenReturnDataTypeNameIsPath() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(null);

    // Act
    Path actualCreateClasspathResult = commandlineJava.createClasspath(new Project());

    // Assert
    assertEquals("Path", actualCreateClasspathResult.getDataTypeName());
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isChecked());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link CommandlineJava#createBootclasspath(Project)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createBootclasspath(Project)}
   */
  @Test
  public void testCreateBootclasspath_givenCommandlineJava_thenReturnProjectIsProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    Project p = new Project();

    // Act and Assert
    assertSame(p, commandlineJava.createBootclasspath(p).getProject());
  }

  /**
   * Test {@link CommandlineJava#createBootclasspath(Project)}.
   * <ul>
   *   <li>Then return DataTypeName is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createBootclasspath(Project)}
   */
  @Test
  public void testCreateBootclasspath_thenReturnDataTypeNameIsPath() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(null);

    // Act
    Path actualCreateBootclasspathResult = commandlineJava.createBootclasspath(new Project());

    // Assert
    assertEquals("Path", actualCreateBootclasspathResult.getDataTypeName());
    Location location = actualCreateBootclasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateBootclasspathResult.getDescription());
    assertNull(actualCreateBootclasspathResult.getProject());
    assertNull(actualCreateBootclasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateBootclasspathResult.size());
    assertFalse(actualCreateBootclasspathResult.isReference());
    assertTrue(actualCreateBootclasspathResult.isChecked());
    assertTrue(actualCreateBootclasspathResult.isEmpty());
  }

  /**
   * Test {@link CommandlineJava#createModulepath(Project)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createModulepath(Project)}
   */
  @Test
  public void testCreateModulepath_givenCommandlineJava_thenReturnProjectIsProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    Project p = new Project();

    // Act and Assert
    assertSame(p, commandlineJava.createModulepath(p).getProject());
  }

  /**
   * Test {@link CommandlineJava#createModulepath(Project)}.
   * <ul>
   *   <li>Then return DataTypeName is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createModulepath(Project)}
   */
  @Test
  public void testCreateModulepath_thenReturnDataTypeNameIsPath() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(null);

    // Act
    Path actualCreateModulepathResult = commandlineJava.createModulepath(new Project());

    // Assert
    assertEquals("Path", actualCreateModulepathResult.getDataTypeName());
    Location location = actualCreateModulepathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateModulepathResult.getDescription());
    assertNull(actualCreateModulepathResult.getProject());
    assertNull(actualCreateModulepathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateModulepathResult.size());
    assertFalse(actualCreateModulepathResult.isReference());
    assertTrue(actualCreateModulepathResult.isChecked());
    assertTrue(actualCreateModulepathResult.isEmpty());
  }

  /**
   * Test {@link CommandlineJava#createUpgrademodulepath(Project)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createUpgrademodulepath(Project)}
   */
  @Test
  public void testCreateUpgrademodulepath_givenCommandlineJava_thenReturnProjectIsProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    Project p = new Project();

    // Act and Assert
    assertSame(p, commandlineJava.createUpgrademodulepath(p).getProject());
  }

  /**
   * Test {@link CommandlineJava#createUpgrademodulepath(Project)}.
   * <ul>
   *   <li>Then return DataTypeName is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#createUpgrademodulepath(Project)}
   */
  @Test
  public void testCreateUpgrademodulepath_thenReturnDataTypeNameIsPath() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(null);

    // Act
    Path actualCreateUpgrademodulepathResult = commandlineJava.createUpgrademodulepath(new Project());

    // Assert
    assertEquals("Path", actualCreateUpgrademodulepathResult.getDataTypeName());
    Location location = actualCreateUpgrademodulepathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUpgrademodulepathResult.getDescription());
    assertNull(actualCreateUpgrademodulepathResult.getProject());
    assertNull(actualCreateUpgrademodulepathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateUpgrademodulepathResult.size());
    assertFalse(actualCreateUpgrademodulepathResult.isReference());
    assertTrue(actualCreateUpgrademodulepathResult.isChecked());
    assertTrue(actualCreateUpgrademodulepathResult.isEmpty());
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenCommandlineJava() {
    // Arrange and Act
    String[] actualCommandline = (new CommandlineJava()).getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createBootclasspath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenCommandlineJavaCreateBootclasspathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createClasspath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenCommandlineJavaCreateClasspathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createModulepath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenCommandlineJavaCreateModulepathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createUpgrademodulepath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenCommandlineJavaCreateUpgrademodulepathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#getCommandline()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@link Project#JAVA_1_1} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenProjectAddTargetJava_1_1AndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget(Project.JAVA_1_1, new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String[] actualCommandline = commandlineJava.getCommandline();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        actualCommandline);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineJava() {
    // Arrange and Act
    String actualDescribeCommandResult = (new CommandlineJava()).describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createBootclasspath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineJavaCreateBootclasspathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createClasspath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineJavaCreateClasspathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createModulepath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineJavaCreateModulepathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createUpgrademodulepath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineJavaCreateUpgrademodulepathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) Maxmemory is {@code ant.build.clonevm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineJavaMaxmemoryIsAntBuildClonevm() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setMaxmemory("ant.build.clonevm");
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "' with arguments:\n" + "'-Xmxant.build.clonevm'\n" + "\n"
                + "The ' characters around the executable and arguments are\n" + "not part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeCommand()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@link Project#JAVA_1_1} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenProjectAddTargetJava_1_1AndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget(Project.JAVA_1_1, new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act
    String actualDescribeCommandResult = commandlineJava.describeCommand();

    // Assert
    assertEquals(
        String.join("", "Executing '", Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
            "'\nThe ' characters around the executable and arguments are\nnot part of the command.\n"),
        actualDescribeCommandResult);
  }

  /**
   * Test {@link CommandlineJava#describeJavaCommand()}.
   * <p>
   * Method under test: {@link CommandlineJava#describeJavaCommand()}
   */
  @Test
  public void testDescribeJavaCommand() {
    // Arrange, Act and Assert
    assertEquals("", (new CommandlineJava()).describeJavaCommand());
  }

  /**
   * Test {@link CommandlineJava#getActualVMCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getActualVMCommand()}
   */
  @Test
  public void testGetActualVMCommand_givenCommandlineJava_thenReturnArrayLengthIsZero() {
    // Arrange and Act
    Commandline actualActualVMCommand = (new CommandlineJava()).getActualVMCommand();

    // Assert
    assertEquals(0, actualActualVMCommand.getArguments().length);
    assertEquals(1, actualActualVMCommand.size());
    assertEquals(1, actualActualVMCommand.getCommandline().length);
    assertFalse(actualActualVMCommand.iterator().hasNext());
  }

  /**
   * Test {@link CommandlineJava#getActualVMCommand()}.
   * <ul>
   *   <li>Then return second element is {@code -mxfoo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getActualVMCommand()}
   */
  @Test
  public void testGetActualVMCommand_thenReturnSecondElementIsMxfoo() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setMaxmemory("foo");
    commandlineJava.setVmversion("1.1");

    // Act
    Commandline actualActualVMCommand = commandlineJava.getActualVMCommand();

    // Assert
    String[] commandline = actualActualVMCommand.getCommandline();
    assertEquals("-mxfoo", commandline[1]);
    Iterator<Argument> iteratorResult = actualActualVMCommand.iterator();
    Argument nextResult = iteratorResult.next();
    Location location = nextResult.getLocation();
    assertNull(location.getFileName());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(2, actualActualVMCommand.size());
    assertEquals(2, commandline.length);
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"-mxfoo"}, actualActualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-mxfoo"}, nextResult.getParts());
  }

  /**
   * Test {@link CommandlineJava#getActualVMCommand()}.
   * <ul>
   *   <li>Then return second element is {@code -XmxMax}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getActualVMCommand()}
   */
  @Test
  public void testGetActualVMCommand_thenReturnSecondElementIsXmxMax() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setMaxmemory("Max");

    // Act
    Commandline actualActualVMCommand = commandlineJava.getActualVMCommand();

    // Assert
    String[] commandline = actualActualVMCommand.getCommandline();
    assertEquals("-XmxMax", commandline[1]);
    Iterator<Argument> iteratorResult = actualActualVMCommand.iterator();
    Argument nextResult = iteratorResult.next();
    Location location = nextResult.getLocation();
    assertNull(location.getFileName());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(2, actualActualVMCommand.size());
    assertEquals(2, commandline.length);
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"-XmxMax"}, actualActualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-XmxMax"}, nextResult.getParts());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) addSyspropertyset {@link PropertySet} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenCommandlineJavaAddSyspropertysetPropertySet_thenReturnOne() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(1, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) Assertions is {@link Assertions} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenCommandlineJavaAssertionsIsAssertions_thenReturnOne() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setAssertions(new Assertions());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(1, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createBootclasspath {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenCommandlineJavaCreateBootclasspathProject_thenReturnOne() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(1, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createClasspath {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenCommandlineJavaCreateClasspathProject_thenReturnOne() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(new Project());
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(1, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) Maxmemory is {@code ant.build.clonevm}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenCommandlineJavaMaxmemoryIsAntBuildClonevm_thenReturnTwo() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setMaxmemory("ant.build.clonevm");
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(2, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenCommandlineJava_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new CommandlineJava()).size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenProjectAddBuildListenerAntClassLoader_thenReturnOne() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(1, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ignore} and {@link Target#Target()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#size()}
   */
  @Test
  public void testSize_givenProjectAddTargetIgnoreAndTarget_thenReturnOne() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("ignore", new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);
    commandlineJava.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertEquals(1, commandlineJava.size());
  }

  /**
   * Test {@link CommandlineJava#getVmCommand()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getVmCommand()}
   */
  @Test
  public void testGetVmCommand_givenCommandlineJava_thenReturnArrayLengthIsZero() {
    // Arrange and Act
    Commandline actualVmCommand = (new CommandlineJava()).getVmCommand();

    // Assert
    assertEquals(0, actualVmCommand.getArguments().length);
    assertEquals(1, actualVmCommand.size());
    assertEquals(1, actualVmCommand.getCommandline().length);
    assertFalse(actualVmCommand.iterator().hasNext());
  }

  /**
   * Test {@link CommandlineJava#getVmCommand()}.
   * <ul>
   *   <li>Then return second element is {@code -mxfoo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getVmCommand()}
   */
  @Test
  public void testGetVmCommand_thenReturnSecondElementIsMxfoo() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setMaxmemory("foo");
    commandlineJava.setVmversion("1.1");

    // Act
    Commandline actualVmCommand = commandlineJava.getVmCommand();

    // Assert
    String[] commandline = actualVmCommand.getCommandline();
    assertEquals("-mxfoo", commandline[1]);
    Iterator<Argument> iteratorResult = actualVmCommand.iterator();
    Argument nextResult = iteratorResult.next();
    Location location = nextResult.getLocation();
    assertNull(location.getFileName());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(2, actualVmCommand.size());
    assertEquals(2, commandline.length);
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"-mxfoo"}, actualVmCommand.getArguments());
    assertArrayEquals(new String[]{"-mxfoo"}, nextResult.getParts());
  }

  /**
   * Test {@link CommandlineJava#getVmCommand()}.
   * <ul>
   *   <li>Then return second element is {@code -XmxMax}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#getVmCommand()}
   */
  @Test
  public void testGetVmCommand_thenReturnSecondElementIsXmxMax() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.setMaxmemory("Max");

    // Act
    Commandline actualVmCommand = commandlineJava.getVmCommand();

    // Assert
    String[] commandline = actualVmCommand.getCommandline();
    assertEquals("-XmxMax", commandline[1]);
    Iterator<Argument> iteratorResult = actualVmCommand.iterator();
    Argument nextResult = iteratorResult.next();
    Location location = nextResult.getLocation();
    assertNull(location.getFileName());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(2, actualVmCommand.size());
    assertEquals(2, commandline.length);
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"-XmxMax"}, actualVmCommand.getArguments());
    assertArrayEquals(new String[]{"-XmxMax"}, nextResult.getParts());
  }

  /**
   * Test {@link CommandlineJava#clone()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>Then return Bootclasspath is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#clone()}
   */
  @Test
  public void testClone_givenCommandlineJava_thenReturnBootclasspathIsNull() throws CloneNotSupportedException {
    // Arrange and Act
    Object actualCloneResult = (new CommandlineJava()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof CommandlineJava);
    assertNull(((CommandlineJava) actualCloneResult).getBootclasspath());
    assertNull(((CommandlineJava) actualCloneResult).getClasspath());
    assertNull(((CommandlineJava) actualCloneResult).getModulepath());
    assertNull(((CommandlineJava) actualCloneResult).getUpgrademodulepath());
  }

  /**
   * Test {@link CommandlineJava#clone()}.
   * <ul>
   *   <li>Then return Assertions DataTypeName is {@code Assertions}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#clone()}
   */
  @Test
  public void testClone_thenReturnAssertionsDataTypeNameIsAssertions() throws CloneNotSupportedException {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(null);
    commandlineJava.createBootclasspath(null);
    commandlineJava.createModulepath(null);
    commandlineJava.createUpgrademodulepath(null);
    commandlineJava.setAssertions(new Assertions());

    // Act
    Object actualCloneResult = commandlineJava.clone();

    // Assert
    assertTrue(actualCloneResult instanceof CommandlineJava);
    Assertions assertions = ((CommandlineJava) actualCloneResult).getAssertions();
    assertEquals("Assertions", assertions.getDataTypeName());
    assertNull(assertions.getDescription());
    assertNull(assertions.getProject());
    assertNull(assertions.getRefid());
    assertEquals(0, assertions.size());
    assertFalse(assertions.isReference());
    assertTrue(assertions.isChecked());
  }

  /**
   * Test {@link CommandlineJava#clone()}.
   * <ul>
   *   <li>Then return Bootclasspath Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#clone()}
   */
  @Test
  public void testClone_thenReturnBootclasspathLocationFileNameIsNull() throws CloneNotSupportedException {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(null);
    commandlineJava.createBootclasspath(null);
    commandlineJava.createModulepath(null);
    commandlineJava.createUpgrademodulepath(null);
    commandlineJava.setAssertions(null);

    // Act
    Object actualCloneResult = commandlineJava.clone();

    // Assert
    assertTrue(actualCloneResult instanceof CommandlineJava);
    Location location = ((CommandlineJava) actualCloneResult).getBootclasspath().getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(location, ((CommandlineJava) actualCloneResult).getClasspath().getLocation());
    assertSame(location, ((CommandlineJava) actualCloneResult).getModulepath().getLocation());
    assertSame(location, ((CommandlineJava) actualCloneResult).getUpgrademodulepath().getLocation());
  }

  /**
   * Test {@link CommandlineJava#haveClasspath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveClasspath()}
   */
  @Test
  public void testHaveClasspath_givenCommandlineJava() {
    // Arrange, Act and Assert
    assertFalse((new CommandlineJava()).haveClasspath());
  }

  /**
   * Test {@link CommandlineJava#haveClasspath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createClasspath {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveClasspath()}
   */
  @Test
  public void testHaveClasspath_givenCommandlineJavaCreateClasspathNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(null);

    // Act and Assert
    assertFalse(commandlineJava.haveClasspath());
  }

  /**
   * Test {@link CommandlineJava#haveClasspath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createClasspath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveClasspath()}
   */
  @Test
  public void testHaveClasspath_givenCommandlineJavaCreateClasspathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(new Project());

    // Act and Assert
    assertFalse(commandlineJava.haveClasspath());
  }

  /**
   * Test {@link CommandlineJava#haveClasspath()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveClasspath()}
   */
  @Test
  public void testHaveClasspath_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("build.sysclasspath", typeClass);
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveClasspath());
  }

  /**
   * Test {@link CommandlineJava#haveClasspath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveClasspath()}
   */
  @Test
  public void testHaveClasspath_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveClasspath());
  }

  /**
   * Test {@link CommandlineJava#haveClasspath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code build.sysclasspath} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveClasspath()}
   */
  @Test
  public void testHaveClasspath_givenProjectAddTargetBuildSysclasspathAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("build.sysclasspath", new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createClasspath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveClasspath());
  }

  /**
   * Test {@link CommandlineJava#haveBootclasspath(boolean)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createBootclasspath {@code null}.</li>
   *   <li>When {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveBootclasspath(boolean)}
   */
  @Test
  public void testHaveBootclasspath_givenCommandlineJavaCreateBootclasspathNull_whenFalse() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(null);
    commandlineJava.setVmversion("1.1");
    commandlineJava.setCloneVm(false);

    // Act and Assert
    assertFalse(commandlineJava.haveBootclasspath(false));
  }

  /**
   * Test {@link CommandlineJava#haveBootclasspath(boolean)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createBootclasspath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveBootclasspath(boolean)}
   */
  @Test
  public void testHaveBootclasspath_givenCommandlineJavaCreateBootclasspathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(new Project());

    // Act and Assert
    assertFalse(commandlineJava.haveBootclasspath(true));
  }

  /**
   * Test {@link CommandlineJava#haveBootclasspath(boolean)}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveBootclasspath(boolean)}
   */
  @Test
  public void testHaveBootclasspath_givenCommandlineJava_whenTrue_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new CommandlineJava()).haveBootclasspath(true));
  }

  /**
   * Test {@link CommandlineJava#haveBootclasspath(boolean)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveBootclasspath(boolean)}
   */
  @Test
  public void testHaveBootclasspath_givenJavaLangObject_whenTrue_thenReturnFalse() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("ant.build.clonevm", typeClass);
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveBootclasspath(true));
  }

  /**
   * Test {@link CommandlineJava#haveBootclasspath(boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveBootclasspath(boolean)}
   */
  @Test
  public void testHaveBootclasspath_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveBootclasspath(true));
  }

  /**
   * Test {@link CommandlineJava#haveBootclasspath(boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.build.clonevm} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveBootclasspath(boolean)}
   */
  @Test
  public void testHaveBootclasspath_givenProjectAddTargetAntBuildClonevmAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("ant.build.clonevm", new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createBootclasspath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveBootclasspath(true));
  }

  /**
   * Test {@link CommandlineJava#haveModulepath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveModulepath()}
   */
  @Test
  public void testHaveModulepath_givenCommandlineJava() {
    // Arrange, Act and Assert
    assertFalse((new CommandlineJava()).haveModulepath());
  }

  /**
   * Test {@link CommandlineJava#haveModulepath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createModulepath {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveModulepath()}
   */
  @Test
  public void testHaveModulepath_givenCommandlineJavaCreateModulepathNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(null);

    // Act and Assert
    assertFalse(commandlineJava.haveModulepath());
  }

  /**
   * Test {@link CommandlineJava#haveModulepath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createModulepath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveModulepath()}
   */
  @Test
  public void testHaveModulepath_givenCommandlineJavaCreateModulepathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(new Project());

    // Act and Assert
    assertFalse(commandlineJava.haveModulepath());
  }

  /**
   * Test {@link CommandlineJava#haveModulepath()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveModulepath()}
   */
  @Test
  public void testHaveModulepath_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("build.sysclasspath", typeClass);
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveModulepath());
  }

  /**
   * Test {@link CommandlineJava#haveModulepath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveModulepath()}
   */
  @Test
  public void testHaveModulepath_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveModulepath());
  }

  /**
   * Test {@link CommandlineJava#haveModulepath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code build.sysclasspath} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveModulepath()}
   */
  @Test
  public void testHaveModulepath_givenProjectAddTargetBuildSysclasspathAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("build.sysclasspath", new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createModulepath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveModulepath());
  }

  /**
   * Test {@link CommandlineJava#haveUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveUpgrademodulepath()}
   */
  @Test
  public void testHaveUpgrademodulepath_givenCommandlineJava() {
    // Arrange, Act and Assert
    assertFalse((new CommandlineJava()).haveUpgrademodulepath());
  }

  /**
   * Test {@link CommandlineJava#haveUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createUpgrademodulepath {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveUpgrademodulepath()}
   */
  @Test
  public void testHaveUpgrademodulepath_givenCommandlineJavaCreateUpgrademodulepathNull() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(null);

    // Act and Assert
    assertFalse(commandlineJava.haveUpgrademodulepath());
  }

  /**
   * Test {@link CommandlineJava#haveUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link CommandlineJava} (default constructor) createUpgrademodulepath {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveUpgrademodulepath()}
   */
  @Test
  public void testHaveUpgrademodulepath_givenCommandlineJavaCreateUpgrademodulepathProject() {
    // Arrange
    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(new Project());

    // Act and Assert
    assertFalse(commandlineJava.haveUpgrademodulepath());
  }

  /**
   * Test {@link CommandlineJava#haveUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveUpgrademodulepath()}
   */
  @Test
  public void testHaveUpgrademodulepath_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("build.sysclasspath", typeClass);
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveUpgrademodulepath());
  }

  /**
   * Test {@link CommandlineJava#haveUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveUpgrademodulepath()}
   */
  @Test
  public void testHaveUpgrademodulepath_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveUpgrademodulepath());
  }

  /**
   * Test {@link CommandlineJava#haveUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code build.sysclasspath} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandlineJava#haveUpgrademodulepath()}
   */
  @Test
  public void testHaveUpgrademodulepath_givenProjectAddTargetBuildSysclasspathAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("build.sysclasspath", new Target());
    p.addBuildListener(new AntClassLoader());

    CommandlineJava commandlineJava = new CommandlineJava();
    commandlineJava.createUpgrademodulepath(p);

    // Act and Assert
    assertFalse(commandlineJava.haveUpgrademodulepath());
  }

  /**
   * Test SysProperties {@link SysProperties#clone()}.
   * <p>
   * Method under test: {@link SysProperties#clone()}
   */
  @Test
  public void testSysPropertiesClone() throws CloneNotSupportedException, BuildException {
    // Arrange and Act
    Object actualCloneResult = (new SysProperties()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof SysProperties);
    assertNull(((SysProperties) actualCloneResult).getVariables());
    assertNull(((SysProperties) actualCloneResult).sys);
    assertEquals(0, ((SysProperties) actualCloneResult).size());
    assertTrue(((SysProperties) actualCloneResult).getVariablesVector().isEmpty());
  }

  /**
   * Test SysProperties {@link SysProperties#getVariables()}.
   * <p>
   * Method under test: {@link SysProperties#getVariables()}
   */
  @Test
  public void testSysPropertiesGetVariables() throws BuildException {
    // Arrange
    Variable resultVar = new Variable();
    resultVar.setValue("42");
    resultVar.setKey("key and value must be specified for environment variables.");

    SysProperties sysProperties = new SysProperties();
    sysProperties.addVariable(resultVar);
    sysProperties.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertArrayEquals(new String[]{"-Dkey and value must be specified for environment variables.=42"},
        sysProperties.getVariables());
  }

  /**
   * Test SysProperties {@link SysProperties#getVariables()}.
   * <ul>
   *   <li>Given {@link SysProperties} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SysProperties#getVariables()}
   */
  @Test
  public void testSysPropertiesGetVariables_givenSysProperties_thenReturnNull() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new SysProperties()).getVariables());
  }

  /**
   * Test SysProperties {@link SysProperties#getVariables()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SysProperties#getVariables()}
   */
  @Test
  public void testSysPropertiesGetVariables_thenReturnNull() throws BuildException {
    // Arrange
    SysProperties sysProperties = new SysProperties();
    sysProperties.addSyspropertyset(new PropertySet());

    // Act and Assert
    assertNull(sysProperties.getVariables());
  }

  /**
   * Test SysProperties new {@link SysProperties} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SysProperties}
   */
  @Test
  public void testSysPropertiesNewSysProperties() throws BuildException {
    // Arrange and Act
    SysProperties actualSysProperties = new SysProperties();

    // Assert
    assertNull(actualSysProperties.getVariables());
    assertNull(actualSysProperties.sys);
    assertEquals(0, actualSysProperties.size());
    assertTrue(actualSysProperties.getVariablesVector().isEmpty());
  }

  /**
   * Test SysProperties {@link SysProperties#restoreSystem()}.
   * <p>
   * Method under test: {@link SysProperties#restoreSystem()}
   */
  @Test
  public void testSysPropertiesRestoreSystem() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SysProperties()).restoreSystem());
  }
}

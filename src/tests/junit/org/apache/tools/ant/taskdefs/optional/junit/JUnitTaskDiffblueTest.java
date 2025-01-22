package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.ExecuteWatchdog;
import org.apache.tools.ant.taskdefs.LogOutputStream;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTask.ForkMode;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTask.JUnitLogOutputStream;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTask.SummaryAttribute;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTask.TestResultHolder;
import org.apache.tools.ant.types.Assertions;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.CommandlineJava;
import org.apache.tools.ant.types.CommandlineJava.SysProperties;
import org.apache.tools.ant.types.Environment;
import org.apache.tools.ant.types.Environment.Variable;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Permissions;
import org.apache.tools.ant.util.CollectionUtils;
import org.apache.tools.ant.util.CollectionUtils.EmptyEnumeration;
import org.junit.Test;

public class JUnitTaskDiffblueTest {
  /**
   * Test ForkMode {@link ForkMode#getValues()}.
   * <ul>
   *   <li>Given {@link ForkMode#ForkMode()}.</li>
   *   <li>Then return array of {@link String} with {@link ForkMode#ONCE} and {@link ForkMode#PER_TEST}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ForkMode#getValues()}
   */
  @Test
  public void testForkModeGetValues_givenForkMode_thenReturnArrayOfStringWithOnceAndPer_test() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{ForkMode.ONCE, ForkMode.PER_TEST, ForkMode.PER_BATCH}, (new ForkMode()).getValues());
  }

  /**
   * Test ForkMode {@link ForkMode#ForkMode()}.
   * <p>
   * Method under test: {@link ForkMode#ForkMode()}
   */
  @Test
  public void testForkModeNewForkMode() {
    // Arrange and Act
    ForkMode actualForkMode = new ForkMode();

    // Assert
    assertNull(actualForkMode.getValue());
    assertEquals(-1, actualForkMode.getIndex());
  }

  /**
   * Test ForkMode {@link ForkMode#ForkMode(String)}.
   * <ul>
   *   <li>When {@link ForkMode#ONCE}.</li>
   *   <li>Then return Index is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ForkMode#ForkMode(String)}
   */
  @Test
  public void testForkModeNewForkMode_whenOnce_thenReturnIndexIsZero() {
    // Arrange and Act
    ForkMode actualForkMode = new ForkMode(ForkMode.ONCE);

    // Assert
    assertEquals(0, actualForkMode.getIndex());
    assertEquals(ForkMode.ONCE, actualForkMode.getValue());
    assertArrayEquals(new String[]{ForkMode.ONCE, ForkMode.PER_TEST, ForkMode.PER_BATCH}, actualForkMode.getValues());
  }

  /**
   * Test JUnitLogOutputStream {@link JUnitLogOutputStream#JUnitLogOutputStream(Task, int)}.
   * <p>
   * Method under test: {@link JUnitLogOutputStream#JUnitLogOutputStream(Task, int)}
   */
  @Test
  public void testJUnitLogOutputStreamNewJUnitLogOutputStream() {
    // Arrange, Act and Assert
    assertEquals(1, (new JUnitLogOutputStream(new TaskAdapter(), 1)).getMessageLevel());
  }

  /**
   * Test {@link JUnitTask#setMaxmemory(String)}.
   * <p>
   * Method under test: {@link JUnitTask#setMaxmemory(String)}
   */
  @Test
  public void testSetMaxmemory() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act
    jUnitTask.setMaxmemory("Max");

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals("-XmxMax", commandline2[1]);
    assertEquals("org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner", commandline2[2]);
    assertEquals(3, commandline.size());
    assertEquals(3, commandline2.length);
  }

  /**
   * Test {@link JUnitTask#setMaxmemory(String)}.
   * <ul>
   *   <li>Then {@link JUnitTask#JUnitTask()} Commandline VmCommand iterator next Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#setMaxmemory(String)}
   */
  @Test
  public void testSetMaxmemory_thenJUnitTaskCommandlineVmCommandIteratorNextDescriptionIsNull() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addSysproperty(new Variable());

    // Act
    jUnitTask.setMaxmemory("Max");

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    Commandline vmCommand = commandline.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-XmxMax", commandline2[1]);
    Iterator<Argument> iteratorResult = vmCommand.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(2, vmCommand.size());
    assertEquals(2, commandline2.length);
    assertEquals(4, commandline.size());
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"-XmxMax"}, vmCommand.getArguments());
    assertArrayEquals(new String[]{"-XmxMax"}, nextResult.getParts());
  }

  /**
   * Test {@link JUnitTask#setJvm(String)}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addSysproperty {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#setJvm(String)}
   */
  @Test
  public void testSetJvm_givenJUnitTaskAddSyspropertyVariable() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addSysproperty(new Variable());

    // Act
    jUnitTask.setJvm("42");

    // Assert
    Commandline vmCommand = jUnitTask.getCommandline().getVmCommand();
    assertEquals("42", vmCommand.getExecutable());
    assertArrayEquals(new String[]{"42"}, vmCommand.getCommandline());
  }

  /**
   * Test {@link JUnitTask#setJvm(String)}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then first element is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#setJvm(String)}
   */
  @Test
  public void testSetJvm_givenJUnitTask_when42_thenFirstElementIs42() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act
    jUnitTask.setJvm("42");

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    Commandline vmCommand = commandline.getVmCommand();
    assertEquals("42", vmCommand.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("42", commandline2[0]);
    assertEquals(2, commandline2.length);
    assertArrayEquals(new String[]{"42"}, vmCommand.getCommandline());
  }

  /**
   * Test {@link JUnitTask#setJvm(String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#setJvm(String)}
   */
  @Test
  public void testSetJvm_whenEmptyString() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act
    jUnitTask.setJvm("");

    // Assert that nothing has changed
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals(2, commandline2.length);
    String expectedExecutable = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    Commandline vmCommand = commandline.getVmCommand();
    assertEquals(expectedExecutable, vmCommand.getExecutable());
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", "java").toString(), commandline2[0]);
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        vmCommand.getCommandline());
  }

  /**
   * Test {@link JUnitTask#setJvm(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#setJvm(String)}
   */
  @Test
  public void testSetJvm_whenNull() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act
    jUnitTask.setJvm(null);

    // Assert that nothing has changed
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals(2, commandline2.length);
    String expectedExecutable = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    Commandline vmCommand = commandline.getVmCommand();
    assertEquals(expectedExecutable, vmCommand.getExecutable());
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", "java").toString(), commandline2[0]);
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString()},
        vmCommand.getCommandline());
  }

  /**
   * Test {@link JUnitTask#addSysproperty(Variable)}.
   * <p>
   * Method under test: {@link JUnitTask#addSysproperty(Variable)}
   */
  @Test
  public void testAddSysproperty() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Variable sysp = new Variable();

    // Act
    jUnitTask.addSysproperty(sysp);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(3, commandline.size());
    assertSame(sysp, variablesVector.get(0));
  }

  /**
   * Test {@link JUnitTask#addSysproperty(Variable)}.
   * <p>
   * Method under test: {@link JUnitTask#addSysproperty(Variable)}
   */
  @Test
  public void testAddSysproperty2() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Variable sysp = new Variable();
    jUnitTask.addSysproperty(sysp);
    Variable sysp2 = new Variable();

    // Act
    jUnitTask.addSysproperty(sysp2);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(2, variablesVector.size());
    assertEquals(2, systemProperties.size());
    assertEquals(4, commandline.size());
    assertSame(sysp, variablesVector.get(0));
    assertSame(sysp2, variablesVector.get(1));
  }

  /**
   * Test {@link JUnitTask#addConfiguredSysproperty(Variable)}.
   * <p>
   * Method under test: {@link JUnitTask#addConfiguredSysproperty(Variable)}
   */
  @Test
  public void testAddConfiguredSysproperty() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(new Project());

    Variable sysp = new Variable();
    sysp.setValue("42");
    sysp.setKey("key and value must be specified for environment variables.");

    // Act
    jUnitTask.addConfiguredSysproperty(sysp);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals("-Dkey and value must be specified for environment variables.=42", commandline2[1]);
    assertEquals("org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner", commandline2[2]);
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(3, commandline.size());
    assertEquals(3, commandline2.length);
    assertSame(sysp, variablesVector.get(0));
    assertArrayEquals(new String[]{"-Dkey and value must be specified for environment variables.=42"},
        systemProperties.getVariables());
  }

  /**
   * Test {@link JUnitTask#addConfiguredSysproperty(Variable)}.
   * <p>
   * Method under test: {@link JUnitTask#addConfiguredSysproperty(Variable)}
   */
  @Test
  public void testAddConfiguredSysproperty2() throws Exception {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    JUnitTask jUnitTask = new JUnitTask();
    Variable sysp = new Variable();
    jUnitTask.addSysproperty(sysp);
    jUnitTask.setProject(project);

    Variable sysp2 = new Variable();
    sysp2.setValue("42");
    sysp2.setKey("key and value must be specified for environment variables.");

    // Act
    jUnitTask.addConfiguredSysproperty(sysp2);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(2, variablesVector.size());
    assertEquals(2, systemProperties.size());
    assertEquals(4, commandline.size());
    assertSame(sysp, variablesVector.get(0));
    assertSame(sysp2, variablesVector.get(1));
  }

  /**
   * Test {@link JUnitTask#addConfiguredSysproperty(Variable)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addConfiguredSysproperty(Variable)}
   */
  @Test
  public void testAddConfiguredSysproperty_givenJavaLangObject() throws Exception {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("sysproperty added : ", typeClass);
    project.addBuildListener(new AntClassLoader());

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    Variable sysp = new Variable();
    sysp.setValue("42");
    sysp.setKey("key and value must be specified for environment variables.");

    // Act
    jUnitTask.addConfiguredSysproperty(sysp);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals("-Dkey and value must be specified for environment variables.=42", commandline2[1]);
    assertEquals("org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner", commandline2[2]);
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(3, commandline.size());
    assertEquals(3, commandline2.length);
    assertSame(sysp, variablesVector.get(0));
    assertArrayEquals(new String[]{"-Dkey and value must be specified for environment variables.=42"},
        systemProperties.getVariables());
  }

  /**
   * Test {@link JUnitTask#addConfiguredSysproperty(Variable)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addConfiguredSysproperty(Variable)}
   */
  @Test
  public void testAddConfiguredSysproperty_givenProjectAddBuildListenerAntClassLoader() throws Exception {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    Variable sysp = new Variable();
    sysp.setValue("42");
    sysp.setKey("key and value must be specified for environment variables.");

    // Act
    jUnitTask.addConfiguredSysproperty(sysp);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals("-Dkey and value must be specified for environment variables.=42", commandline2[1]);
    assertEquals("org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner", commandline2[2]);
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(3, commandline.size());
    assertEquals(3, commandline2.length);
    assertSame(sysp, variablesVector.get(0));
    assertArrayEquals(new String[]{"-Dkey and value must be specified for environment variables.=42"},
        systemProperties.getVariables());
  }

  /**
   * Test {@link JUnitTask#addConfiguredSysproperty(Variable)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addConfiguredSysproperty(Variable)}
   */
  @Test
  public void testAddConfiguredSysproperty_givenProjectAddBuildListenerDefaultLogger() throws Exception {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    Variable sysp = new Variable();
    sysp.setValue("42");
    sysp.setKey("key and value must be specified for environment variables.");

    // Act
    jUnitTask.addConfiguredSysproperty(sysp);

    // Assert
    CommandlineJava commandline = jUnitTask.getCommandline();
    String[] commandline2 = commandline.getCommandline();
    assertEquals("-Dkey and value must be specified for environment variables.=42", commandline2[1]);
    assertEquals("org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner", commandline2[2]);
    SysProperties systemProperties = commandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(3, commandline.size());
    assertEquals(3, commandline2.length);
    assertSame(sysp, variablesVector.get(0));
    assertArrayEquals(new String[]{"-Dkey and value must be specified for environment variables.=42"},
        systemProperties.getVariables());
  }

  /**
   * Test {@link JUnitTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addSysproperty {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJUnitTaskAddSyspropertyVariable() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addSysproperty(new Variable());

    // Act
    Path actualCreateClasspathResult = jUnitTask.createClasspath();

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
   * Test {@link JUnitTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJUnitTaskProjectIsProject_thenReturnProjectIsProject() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Project project = new Project();
    jUnitTask.setProject(project);

    // Act and Assert
    assertSame(project, jUnitTask.createClasspath().getProject());
  }

  /**
   * Test {@link JUnitTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJUnitTask_thenReturnLocationFileNameIsNull() throws Exception {
    // Arrange and Act
    Path actualCreateClasspathResult = (new JUnitTask()).createClasspath();

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
   * Test {@link JUnitTask#createBootclasspath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addSysproperty {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_givenJUnitTaskAddSyspropertyVariable() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addSysproperty(new Variable());

    // Act
    Path actualCreateBootclasspathResult = jUnitTask.createBootclasspath();

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
   * Test {@link JUnitTask#createBootclasspath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_givenJUnitTask_thenReturnLocationFileNameIsNull() throws Exception {
    // Arrange and Act
    Path actualCreateBootclasspathResult = (new JUnitTask()).createBootclasspath();

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
   * Test {@link JUnitTask#createBootclasspath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createBootclasspath()}
   */
  @Test
  public void testCreateBootclasspath_thenReturnProjectIsProject() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Project project = new Project();
    jUnitTask.setProject(project);

    // Act and Assert
    assertSame(project, jUnitTask.createBootclasspath().getProject());
  }

  /**
   * Test {@link JUnitTask#createModulepath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addSysproperty {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createModulepath()}
   */
  @Test
  public void testCreateModulepath_givenJUnitTaskAddSyspropertyVariable() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addSysproperty(new Variable());

    // Act
    Path actualCreateModulepathResult = jUnitTask.createModulepath();

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
   * Test {@link JUnitTask#createModulepath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createModulepath()}
   */
  @Test
  public void testCreateModulepath_givenJUnitTaskProjectIsProject_thenReturnProjectIsProject() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Project project = new Project();
    jUnitTask.setProject(project);

    // Act and Assert
    assertSame(project, jUnitTask.createModulepath().getProject());
  }

  /**
   * Test {@link JUnitTask#createModulepath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createModulepath()}
   */
  @Test
  public void testCreateModulepath_givenJUnitTask_thenReturnLocationFileNameIsNull() throws Exception {
    // Arrange and Act
    Path actualCreateModulepathResult = (new JUnitTask()).createModulepath();

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
   * Test {@link JUnitTask#createUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addSysproperty {@link Variable} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_givenJUnitTaskAddSyspropertyVariable() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addSysproperty(new Variable());

    // Act
    Path actualCreateUpgrademodulepathResult = jUnitTask.createUpgrademodulepath();

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
   * Test {@link JUnitTask#createUpgrademodulepath()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_givenJUnitTask_thenReturnLocationFileNameIsNull() throws Exception {
    // Arrange and Act
    Path actualCreateUpgrademodulepathResult = (new JUnitTask()).createUpgrademodulepath();

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
   * Test {@link JUnitTask#createUpgrademodulepath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createUpgrademodulepath()}
   */
  @Test
  public void testCreateUpgrademodulepath_thenReturnProjectIsProject() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Project project = new Project();
    jUnitTask.setProject(project);

    // Act and Assert
    assertSame(project, jUnitTask.createUpgrademodulepath().getProject());
  }

  /**
   * Test {@link JUnitTask#addTest(JUnitTest)}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} ErrorProperty is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addTest(JUnitTest)}
   */
  @Test
  public void testAddTest_givenJUnitTask_thenJUnitTestWithNameErrorPropertyIsNull() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    JUnitTest test = new JUnitTest("Name");

    // Act
    jUnitTask.addTest(test);

    // Assert that nothing has changed
    assertNull(test.getErrorProperty());
    assertNull(test.getFailureProperty());
  }

  /**
   * Test {@link JUnitTask#addTest(JUnitTest)}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} ErrorProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addTest(JUnitTest)}
   */
  @Test
  public void testAddTest_thenJUnitTestWithNameErrorPropertyIsFoo() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setErrorProperty("foo");
    jUnitTask.setFailureProperty(null);
    JUnitTest test = new JUnitTest("Name");

    // Act
    jUnitTask.addTest(test);

    // Assert
    assertEquals("foo", test.getErrorProperty());
    assertNull(test.getFailureProperty());
  }

  /**
   * Test {@link JUnitTask#addTest(JUnitTest)}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} FailureProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addTest(JUnitTest)}
   */
  @Test
  public void testAddTest_thenJUnitTestWithNameFailurePropertyIsFoo() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setErrorProperty(null);
    jUnitTask.setFailureProperty("foo");
    JUnitTest test = new JUnitTest("Name");

    // Act
    jUnitTask.addTest(test);

    // Assert
    assertEquals("foo", test.getFailureProperty());
    assertNull(test.getErrorProperty());
  }

  /**
   * Test {@link JUnitTask#createBatchTest()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return ErrorProperty is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createBatchTest()}
   */
  @Test
  public void testCreateBatchTest_givenJUnitTask_thenReturnErrorPropertyIsNull() throws Exception {
    // Arrange and Act
    BatchTest actualCreateBatchTestResult = (new JUnitTask()).createBatchTest();

    // Assert
    assertNull(actualCreateBatchTestResult.destDir);
    assertNull(actualCreateBatchTestResult.getIfCondition());
    assertNull(actualCreateBatchTestResult.getUnlessCondition());
    assertNull(actualCreateBatchTestResult.getErrorProperty());
    assertNull(actualCreateBatchTestResult.getFailureProperty());
    assertNull(actualCreateBatchTestResult.getTodir());
    assertNull(actualCreateBatchTestResult.ifProperty);
    assertNull(actualCreateBatchTestResult.unlessProperty);
    assertFalse(actualCreateBatchTestResult.getFork());
    assertFalse(actualCreateBatchTestResult.getHaltonerror());
    assertFalse(actualCreateBatchTestResult.getHaltonfailure());
    assertFalse(actualCreateBatchTestResult.isSkipNonTests());
    assertTrue(actualCreateBatchTestResult.formatters.isEmpty());
    assertTrue(actualCreateBatchTestResult.getFiltertrace());
  }

  /**
   * Test {@link JUnitTask#createBatchTest()}.
   * <ul>
   *   <li>Then return ErrorProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createBatchTest()}
   */
  @Test
  public void testCreateBatchTest_thenReturnErrorPropertyIsFoo() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setErrorProperty("foo");
    jUnitTask.setFailureProperty(null);

    // Act
    BatchTest actualCreateBatchTestResult = jUnitTask.createBatchTest();

    // Assert
    assertEquals("foo", actualCreateBatchTestResult.getErrorProperty());
    assertNull(actualCreateBatchTestResult.destDir);
    assertNull(actualCreateBatchTestResult.getIfCondition());
    assertNull(actualCreateBatchTestResult.getUnlessCondition());
    assertNull(actualCreateBatchTestResult.getFailureProperty());
    assertNull(actualCreateBatchTestResult.getTodir());
    assertNull(actualCreateBatchTestResult.ifProperty);
    assertNull(actualCreateBatchTestResult.unlessProperty);
    assertFalse(actualCreateBatchTestResult.getFork());
    assertFalse(actualCreateBatchTestResult.getHaltonerror());
    assertFalse(actualCreateBatchTestResult.getHaltonfailure());
    assertFalse(actualCreateBatchTestResult.isSkipNonTests());
    assertTrue(actualCreateBatchTestResult.formatters.isEmpty());
    assertTrue(actualCreateBatchTestResult.getFiltertrace());
  }

  /**
   * Test {@link JUnitTask#createBatchTest()}.
   * <ul>
   *   <li>Then return FailureProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createBatchTest()}
   */
  @Test
  public void testCreateBatchTest_thenReturnFailurePropertyIsFoo() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setErrorProperty(null);
    jUnitTask.setFailureProperty("foo");

    // Act
    BatchTest actualCreateBatchTestResult = jUnitTask.createBatchTest();

    // Assert
    assertEquals("foo", actualCreateBatchTestResult.getFailureProperty());
    assertNull(actualCreateBatchTestResult.destDir);
    assertNull(actualCreateBatchTestResult.getIfCondition());
    assertNull(actualCreateBatchTestResult.getUnlessCondition());
    assertNull(actualCreateBatchTestResult.getErrorProperty());
    assertNull(actualCreateBatchTestResult.getTodir());
    assertNull(actualCreateBatchTestResult.ifProperty);
    assertNull(actualCreateBatchTestResult.unlessProperty);
    assertFalse(actualCreateBatchTestResult.getFork());
    assertFalse(actualCreateBatchTestResult.getHaltonerror());
    assertFalse(actualCreateBatchTestResult.getHaltonfailure());
    assertFalse(actualCreateBatchTestResult.isSkipNonTests());
    assertTrue(actualCreateBatchTestResult.formatters.isEmpty());
    assertTrue(actualCreateBatchTestResult.getFiltertrace());
  }

  /**
   * Test {@link JUnitTask#addAssertions(Assertions)}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addAssertions {@link Assertions} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addAssertions(Assertions)}
   */
  @Test
  public void testAddAssertions_givenJUnitTaskAddAssertionsAssertions_thenThrowBuildException() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addAssertions(new Assertions());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.addAssertions(new Assertions()));
  }

  /**
   * Test {@link JUnitTask#addAssertions(Assertions)}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then {@link JUnitTask#JUnitTask()} Commandline Assertions is {@link Assertions} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#addAssertions(Assertions)}
   */
  @Test
  public void testAddAssertions_givenJUnitTask_thenJUnitTaskCommandlineAssertionsIsAssertions() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Assertions asserts = new Assertions();

    // Act
    jUnitTask.addAssertions(asserts);

    // Assert
    assertSame(asserts, jUnitTask.getCommandline().getAssertions());
  }

  /**
   * Test {@link JUnitTask#createPermissions()}.
   * <p>
   * Method under test: {@link JUnitTask#createPermissions()}
   */
  @Test
  public void testCreatePermissions() throws Exception {
    // Arrange and Act
    Permissions actualCreatePermissionsResult = (new JUnitTask()).createPermissions();

    // Assert
    Location location = actualCreatePermissionsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePermissionsResult.getDescription());
    assertNull(actualCreatePermissionsResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link JUnitTask#JUnitTask()}.
   * <p>
   * Method under test: default or parameterless constructor of {@link JUnitTask}
   */
  @Test
  public void testNewJUnitTask() throws Exception {
    // Arrange and Act
    JUnitTask actualJUnitTask = new JUnitTask();

    // Assert
    assertTrue(actualJUnitTask.getDefaultOutput() instanceof LogOutputStream);
    assertNull(actualJUnitTask.getDescription());
    assertNull(actualJUnitTask.getTaskName());
    assertNull(actualJUnitTask.getTaskType());
    assertNull(actualJUnitTask.getProject());
    assertNull(actualJUnitTask.getOwningTarget());
  }

  /**
   * Test {@link JUnitTask#getEnableTestListenerEvents()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getEnableTestListenerEvents()}
   */
  @Test
  public void testGetEnableTestListenerEvents_givenJUnitTaskProjectIsProject_thenReturnFalse() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(new Project());

    // Act and Assert
    assertFalse(jUnitTask.getEnableTestListenerEvents());
  }

  /**
   * Test {@link JUnitTask#getEnableTestListenerEvents()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getEnableTestListenerEvents()}
   */
  @Test
  public void testGetEnableTestListenerEvents_givenJavaLangObject_thenReturnFalse() throws Exception {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    // Act and Assert
    assertFalse(jUnitTask.getEnableTestListenerEvents());
  }

  /**
   * Test {@link JUnitTask#getEnableTestListenerEvents()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getEnableTestListenerEvents()}
   */
  @Test
  public void testGetEnableTestListenerEvents_givenProjectAddBuildListenerAntClassLoader() throws Exception {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    // Act and Assert
    assertFalse(jUnitTask.getEnableTestListenerEvents());
  }

  /**
   * Test {@link JUnitTask#getEnableTestListenerEvents()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getEnableTestListenerEvents()}
   */
  @Test
  public void testGetEnableTestListenerEvents_givenProjectAddTargetAntPropertyHelperAndTarget() throws Exception {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    // Act and Assert
    assertFalse(jUnitTask.getEnableTestListenerEvents());
  }

  /**
   * Test {@link JUnitTask#execute(JUnitTest)} with {@code arg}.
   * <p>
   * Method under test: {@link JUnitTask#execute(JUnitTest)}
   */
  @Test
  public void testExecuteWithArg() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute(new JUnitTest("null", true, true, true)));
  }

  /**
   * Test {@link JUnitTask#execute(JUnitTest)} with {@code arg}.
   * <p>
   * Method under test: {@link JUnitTask#execute(JUnitTest)}
   */
  @Test
  public void testExecuteWithArg2() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute(new JUnitTest("", true, true, true)));
  }

  /**
   * Test {@link JUnitTask#execute(JUnitTest, int)} with {@code arg}, {@code thread}.
   * <p>
   * Method under test: {@link JUnitTask#execute(JUnitTest, int)}
   */
  @Test
  public void testExecuteWithArgThread() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute(new JUnitTest("null", true, true, true), 1));
  }

  /**
   * Test {@link JUnitTask#execute(JUnitTest, int)} with {@code arg}, {@code thread}.
   * <p>
   * Method under test: {@link JUnitTask#execute(JUnitTest, int)}
   */
  @Test
  public void testExecuteWithArgThread2() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute(new JUnitTest("", true, true, true), 1));
  }

  /**
   * Test {@link JUnitTask#execute(JUnitTest, int)} with {@code arg}, {@code thread}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>When {@link JUnitTest#JUnitTest()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#execute(JUnitTest, int)}
   */
  @Test
  public void testExecuteWithArgThread_givenJUnitTask_whenJUnitTest_thenThrowBuildException() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute(new JUnitTest(), 1));
  }

  /**
   * Test {@link JUnitTask#execute(JUnitTest)} with {@code arg}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>When {@link JUnitTest#JUnitTest()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#execute(JUnitTest)}
   */
  @Test
  public void testExecuteWithArg_givenJUnitTask_whenJUnitTest_thenThrowBuildException() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute(new JUnitTest()));
  }

  /**
   * Test {@link JUnitTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addTest {@link JUnitTest#JUnitTest(String)} with name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitTaskAddTestJUnitTestWithNameIsEmptyString() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(new Project());
    jUnitTask.addTest(new JUnitTest(""));

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute());
  }

  /**
   * Test {@link JUnitTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addTest {@link JUnitTest#JUnitTest(String)} with name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitTaskAddTestJUnitTestWithNameIsEmptyString2() throws Exception {
    // Arrange
    JUnitTest test = new JUnitTest("java");
    test.setFailureProperty("/bin");

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setFork(true);
    jUnitTask.addTest(new JUnitTest(""));
    jUnitTask.setThreads(3);
    jUnitTask.addTest(test);
    jUnitTask.addTest(new JUnitTest("java"));

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute());
  }

  /**
   * Test {@link JUnitTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addTest {@link JUnitTest#JUnitTest(String)} with name is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitTaskAddTestJUnitTestWithNameIsNull_thenThrowBuildException() throws Exception {
    // Arrange
    JUnitTest test = new JUnitTest("java");
    test.setFailureProperty("/bin");

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setFork(true);
    jUnitTask.addTest(new JUnitTest(null));
    jUnitTask.setThreads(3);
    jUnitTask.addTest(test);
    jUnitTask.addTest(new JUnitTest("java"));

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute());
  }

  /**
   * Test {@link JUnitTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} addTest {@link JUnitTest#JUnitTest()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitTaskAddTestJUnitTest_thenThrowBuildException() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.addTest(new JUnitTest());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.execute());
  }

  /**
   * Test {@link JUnitTask#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws Exception {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(project);

    // Act and Assert
    assertEquals(3, jUnitTask.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, jUnitTask.getProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link JUnitTask#createWatchdog()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} Timeout is one.</li>
   *   <li>Then return not Watching.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createWatchdog()}
   */
  @Test
  public void testCreateWatchdog_givenJUnitTaskTimeoutIsOne_thenReturnNotWatching() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setTimeout(1);

    // Act
    ExecuteWatchdog actualCreateWatchdogResult = jUnitTask.createWatchdog();

    // Assert
    assertFalse(actualCreateWatchdogResult.isWatching());
    assertFalse(actualCreateWatchdogResult.killedProcess());
  }

  /**
   * Test {@link JUnitTask#createWatchdog()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#createWatchdog()}
   */
  @Test
  public void testCreateWatchdog_givenJUnitTask_thenReturnNull() throws Exception {
    // Arrange, Act and Assert
    assertNull((new JUnitTask()).createWatchdog());
  }

  /**
   * Test {@link JUnitTask#getDefaultOutput()}.
   * <p>
   * Method under test: {@link JUnitTask#getDefaultOutput()}
   */
  @Test
  public void testGetDefaultOutput() throws Exception {
    // Arrange and Act
    OutputStream actualDefaultOutput = (new JUnitTask()).getDefaultOutput();

    // Assert
    assertTrue(actualDefaultOutput instanceof LogOutputStream);
    assertEquals(2, ((LogOutputStream) actualDefaultOutput).getMessageLevel());
  }

  /**
   * Test {@link JUnitTask#getOutput(FormatterElement, JUnitTest)}.
   * <ul>
   *   <li>Given {@code false}.</li>
   *   <li>When {@link FormatterElement} (default constructor) UseFile is {@code false}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getOutput(FormatterElement, JUnitTest)}
   */
  @Test
  public void testGetOutput_givenFalse_whenFormatterElementUseFileIsFalse_thenReturnNull() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    FormatterElement fe = new FormatterElement();
    fe.setUseFile(false);

    JUnitTest test = new JUnitTest("Name");
    test.setOutfile(null);

    // Act and Assert
    assertNull(jUnitTask.getOutput(fe, test));
  }

  /**
   * Test {@link JUnitTask#getOutput(FormatterElement, JUnitTest)}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Name is {@code IGNORETHISnull}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getOutput(FormatterElement, JUnitTest)}
   */
  @Test
  public void testGetOutput_givenJUnitTaskProjectIsProject_thenReturnNameIsIGNORETHISnull() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setProject(new Project());
    FormatterElement fe = new FormatterElement();

    // Act
    File actualOutput = jUnitTask.getOutput(fe, new JUnitTest("Name"));

    // Assert
    assertEquals("IGNORETHISnull", actualOutput.getName());
    assertTrue(actualOutput.isAbsolute());
  }

  /**
   * Test {@link JUnitTask#getCommandline()}.
   * <ul>
   *   <li>Given {@link JUnitTask#JUnitTask()}.</li>
   *   <li>Then return SystemProperties Variables is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getCommandline()}
   */
  @Test
  public void testGetCommandline_givenJUnitTask_thenReturnSystemPropertiesVariablesIsNull() throws Exception {
    // Arrange and Act
    CommandlineJava actualCommandline = (new JUnitTask()).getCommandline();

    // Assert
    SysProperties systemProperties = actualCommandline.getSystemProperties();
    assertNull(systemProperties.getVariables());
    assertEquals(0, systemProperties.size());
    assertEquals(2, actualCommandline.size());
    assertTrue(systemProperties.getVariablesVector().isEmpty());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "java").toString(),
        "org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner"}, actualCommandline.getCommandline());
  }

  /**
   * Test {@link JUnitTask#getCommandline()}.
   * <ul>
   *   <li>Then return SystemProperties VariablesVector size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#getCommandline()}
   */
  @Test
  public void testGetCommandline_thenReturnSystemPropertiesVariablesVectorSizeIsOne() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    Variable sysp = new Variable();
    jUnitTask.addSysproperty(sysp);

    // Act
    CommandlineJava actualCommandline = jUnitTask.getCommandline();

    // Assert
    SysProperties systemProperties = actualCommandline.getSystemProperties();
    Vector<Variable> variablesVector = systemProperties.getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertEquals(1, systemProperties.size());
    assertEquals(3, actualCommandline.size());
    assertSame(sysp, variablesVector.get(0));
  }

  /**
   * Test {@link JUnitTask#executeOrQueue(Enumeration, boolean)}.
   * <ul>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#executeOrQueue(Enumeration, boolean)}
   */
  @Test
  public void testExecuteOrQueue_thenReturnEmpty() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertTrue(jUnitTask.executeOrQueue(new CompoundEnumeration<>(new EmptyEnumeration<>()), true).isEmpty());
  }

  /**
   * Test {@link JUnitTask#actOnTestResult(int, boolean, JUnitTest, String)} with {@code exitValue}, {@code wasKilled}, {@code test}, {@code name}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#actOnTestResult(int, boolean, JUnitTest, String)}
   */
  @Test
  public void testActOnTestResultWithExitValueWasKilledTestName_thenThrowBuildException() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> jUnitTask.actOnTestResult(42, true, new JUnitTest(" FAILED", true, true, true), "Name"));
  }

  /**
   * Test {@link JUnitTask#actOnTestResult(int, boolean, JUnitTest, String)} with {@code exitValue}, {@code wasKilled}, {@code test}, {@code name}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#actOnTestResult(int, boolean, JUnitTest, String)}
   */
  @Test
  public void testActOnTestResultWithExitValueWasKilledTestName_thenThrowBuildException2() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> jUnitTask.actOnTestResult(42, false, new JUnitTest(" FAILED", true, true, true), "Name"));
  }

  /**
   * Test {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)} with {@code result}, {@code test}, {@code name}.
   * <p>
   * Method under test: {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)}
   */
  @Test
  public void testActOnTestResultWithResultTestName() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    TestResultHolder result = new TestResultHolder();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> jUnitTask.actOnTestResult(result, new JUnitTest(" FAILED", true, true, true), "Name"));
  }

  /**
   * Test {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)} with {@code result}, {@code test}, {@code name}.
   * <ul>
   *   <li>When {@link JUnitTest#JUnitTest(String)} with {@code Name} Haltonerror is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)}
   */
  @Test
  public void testActOnTestResultWithResultTestName_whenJUnitTestWithNameHaltonerrorIsTrue() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setLogFailedTests(false);
    jUnitTask.setProject(null);
    TestResultHolder result = new TestResultHolder();
    result.timedOut = true;
    result.crashed = false;
    result.exitCode = 2;

    JUnitTest test = new JUnitTest("Name");
    test.setHaltonerror(true);
    test.setHaltonfailure(false);
    test.setErrorProperty(null);
    test.setFailureProperty(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.actOnTestResult(result, test, "Name"));
  }

  /**
   * Test {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)} with {@code result}, {@code test}, {@code name}.
   * <ul>
   *   <li>When {@link JUnitTest#JUnitTest(String)} with {@code Name} Haltonerror is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)}
   */
  @Test
  public void testActOnTestResultWithResultTestName_whenJUnitTestWithNameHaltonerrorIsTrue2() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    jUnitTask.setLogFailedTests(false);
    jUnitTask.setProject(null);
    TestResultHolder result = new TestResultHolder();
    result.timedOut = false;
    result.crashed = true;
    result.exitCode = 2;

    JUnitTest test = new JUnitTest("Name");
    test.setHaltonerror(true);
    test.setHaltonfailure(false);
    test.setErrorProperty(null);
    test.setFailureProperty(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.actOnTestResult(result, test, "Name"));
  }

  /**
   * Test {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)} with {@code result}, {@code test}, {@code name}.
   * <ul>
   *   <li>When {@link JUnitTest#JUnitTest(String)} with {@code Name} Haltonfailure is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTask#actOnTestResult(TestResultHolder, JUnitTest, String)}
   */
  @Test
  public void testActOnTestResultWithResultTestName_whenJUnitTestWithNameHaltonfailureIsTrue() throws Exception {
    // Arrange
    JUnitTask jUnitTask = new JUnitTask();
    TestResultHolder result = new TestResultHolder();

    JUnitTest test = new JUnitTest("Name");
    test.setHaltonfailure(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitTask.actOnTestResult(result, test, "Name"));
  }

  /**
   * Test SummaryAttribute {@link SummaryAttribute#asBoolean()}.
   * <p>
   * Method under test: {@link SummaryAttribute#asBoolean()}
   */
  @Test
  public void testSummaryAttributeAsBoolean() {
    // Arrange, Act and Assert
    assertFalse((new SummaryAttribute()).asBoolean());
  }

  /**
   * Test SummaryAttribute {@link SummaryAttribute#getValues()}.
   * <p>
   * Method under test: {@link SummaryAttribute#getValues()}
   */
  @Test
  public void testSummaryAttributeGetValues() {
    // Arrange and Act
    String[] actualValues = (new SummaryAttribute()).getValues();

    // Assert
    String toStringResult = Boolean.TRUE.toString();
    assertArrayEquals(new String[]{toStringResult, "yes", Boolean.FALSE.toString(), "no", "on", "off", "withOutAndErr"},
        actualValues);
  }

  /**
   * Test SummaryAttribute new {@link SummaryAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SummaryAttribute}
   */
  @Test
  public void testSummaryAttributeNewSummaryAttribute() {
    // Arrange and Act
    SummaryAttribute actualSummaryAttribute = new SummaryAttribute();

    // Assert
    assertNull(actualSummaryAttribute.getValue());
    assertEquals(-1, actualSummaryAttribute.getIndex());
  }

  /**
   * Test TestResultHolder new {@link TestResultHolder} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TestResultHolder}
   */
  @Test
  public void testTestResultHolderNewTestResultHolder() {
    // Arrange and Act
    TestResultHolder actualTestResultHolder = new TestResultHolder();

    // Assert
    assertEquals(2, actualTestResultHolder.exitCode);
    assertFalse(actualTestResultHolder.crashed);
    assertFalse(actualTestResultHolder.timedOut);
  }
}

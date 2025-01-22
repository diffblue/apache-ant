package org.apache.tools.ant.taskdefs.optional.j2ee;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.CommandlineJava;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JonasHotDeploymentToolDiffblueTest {
  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jonasHotDeploymentTool.setOrb("42");
    Path classpath = new Path(new Project());
    jonasHotDeploymentTool.setClasspath(classpath);

    // Act and Assert
    assertSame(classpath, jonasHotDeploymentTool.getClasspath());
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnLocationFileNameIsNull() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(new ServerDeploy());

    // Act
    Path actualClasspath = jonasHotDeploymentTool.getClasspath();

    // Assert
    Location location = actualClasspath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualClasspath.getDescription());
    assertNull(actualClasspath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualClasspath.size());
    assertFalse(actualClasspath.isReference());
    assertTrue(actualClasspath.isEmpty());
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#Path(Project)} with project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnPathWithProjectIsNull() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    Path classpath = new Path(null);
    jonasHotDeploymentTool.setClasspath(classpath);

    // Act and Assert
    assertSame(classpath, jonasHotDeploymentTool.getClasspath());
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnPathWithProjectIsProject() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    Path classpath = new Path(new Project());
    jonasHotDeploymentTool.setClasspath(classpath);

    // Act and Assert
    assertSame(classpath, jonasHotDeploymentTool.getClasspath());
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnProjectBuildListenersFirstIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);
    Path classpath = new Path(project);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setClasspath(classpath);

    // Act and Assert
    Vector<BuildListener> buildListeners = jonasHotDeploymentTool.getClasspath().getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("_jonas.jar", typeClass);
    project.addBuildListener(new AntClassLoader());
    Path classpath = new Path(project);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setClasspath(classpath);

    // Act and Assert
    Project project2 = jonasHotDeploymentTool.getClasspath().getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertEquals(1, project2.getBuildListeners().size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("_jonas.jar"));
    assertSame(typeClass, dataTypeDefinitions.get("_jonas.jar"));
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnSystemBootClasspath() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setClasspath(Path.systemBootClasspath);

    // Act
    Path actualClasspath = jonasHotDeploymentTool.getClasspath();

    // Assert
    assertSame(actualClasspath.systemBootClasspath, actualClasspath);
  }

  /**
   * Test {@link JonasHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnSystemBootClasspath2() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setClasspath(Path.systemBootClasspath);

    // Act
    Path actualClasspath = jonasHotDeploymentTool.getClasspath();

    // Assert
    assertSame(actualClasspath.systemBootClasspath, actualClasspath);
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    Commandline vmCommand = commandLine.getVmCommand();
    Iterator<Argument> iteratorResult = vmCommand.iterator();
    assertEquals(1, iteratorResult.next().getParts().length);
    assertEquals(1, iteratorResult.next().getParts().length);
    String[] commandline = vmCommand.getCommandline();
    assertEquals(3, commandline.length);
    String[] commandline2 = commandLine.getCommandline();
    assertEquals(5, commandline2.length);
    assertFalse(iteratorResult.hasNext());
    assertEquals(
        String.join("", "-Dinstall.root=", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()),
        commandline[1]);
    assertEquals(
        String.join("", "-Dinstall.root=", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()),
        commandline2[1]);
    assertEquals(
        String.join("", "-Djava.security.policy=",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "config", "java.policy").toString()),
        commandline[2]);
    assertEquals(
        String.join("", "-Djava.security.policy=",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "config", "java.policy").toString()),
        commandline2[2]);
    String joinResult = String.join("", "-Dinstall.root=",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());
    assertArrayEquals(
        new String[]{joinResult,
            String.join("", "-Djava.security.policy=",
                Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "config", "java.policy").toString())},
        vmCommand.getArguments());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link JonasHotDeploymentTool} (default constructor) ClassName is {@link HotDeploymentTool#ACTION_DELETE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenJonasHotDeploymentToolClassNameIsAction_delete() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setClassName(HotDeploymentTool.ACTION_DELETE);
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    String[] arguments = javaCommand.getArguments();
    assertEquals("-r", arguments[0]);
    String[] commandline = javaCommand.getCommandline();
    assertEquals("-r", commandline[0]);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    String[] parts = iteratorResult.next().getParts();
    assertEquals("-r", parts[0]);
    String[] commandline2 = commandLine.getCommandline();
    assertEquals("-r", commandline2[3]);
    assertEquals(2, arguments.length);
    assertEquals(2, commandline.length);
    assertEquals(2, parts.length);
    assertEquals(5, commandline2.length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DEPLOY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsAction_deploy() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    String[] arguments = javaCommand.getArguments();
    assertEquals("-a", arguments[0]);
    String[] commandline = javaCommand.getCommandline();
    assertEquals("-a", commandline[0]);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    String[] parts = iteratorResult.next().getParts();
    assertEquals("-a", parts[0]);
    String[] commandline2 = commandLine.getCommandline();
    assertEquals("-a", commandline2[3]);
    assertEquals(2, arguments.length);
    assertEquals(2, commandline.length);
    assertEquals(2, parts.length);
    assertEquals(5, commandline2.length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_UNDEPLOY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsAction_undeploy() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_UNDEPLOY);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    String[] arguments = javaCommand.getArguments();
    assertEquals("-r", arguments[0]);
    String[] commandline = javaCommand.getCommandline();
    assertEquals("-r", commandline[0]);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    String[] parts = iteratorResult.next().getParts();
    assertEquals("-r", parts[0]);
    String[] commandline2 = commandLine.getCommandline();
    assertEquals("-r", commandline2[3]);
    assertEquals(2, arguments.length);
    assertEquals(2, commandline.length);
    assertEquals(2, parts.length);
    assertEquals(5, commandline2.length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_UPDATE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsAction_update() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_UPDATE);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    String[] arguments = javaCommand.getArguments();
    assertEquals("-a", arguments[0]);
    String[] commandline = javaCommand.getCommandline();
    assertEquals("-a", commandline[0]);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    String[] parts = iteratorResult.next().getParts();
    assertEquals("-a", parts[0]);
    String[] commandline2 = commandLine.getCommandline();
    assertEquals("-a", commandline2[3]);
    assertEquals(2, arguments.length);
    assertEquals(2, commandline.length);
    assertEquals(2, parts.length);
    assertEquals(5, commandline2.length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@code The "action" attribute must be set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsTheActionAttributeMustBeSet() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction("The \"action\" attribute must be set");

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Then first element is {@code -r}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenFirstElementIsR() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    String[] arguments = javaCommand.getArguments();
    assertEquals("-r", arguments[0]);
    String[] commandline = javaCommand.getCommandline();
    assertEquals("-r", commandline[0]);
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    String[] parts = iteratorResult.next().getParts();
    assertEquals("-r", parts[0]);
    String[] commandline2 = commandLine.getCommandline();
    assertEquals("-r", commandline2[3]);
    assertEquals(2, arguments.length);
    assertEquals(2, commandline.length);
    assertEquals(2, parts.length);
    assertEquals(5, commandline2.length);
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Then second element is {@code -r}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenSecondElementIsR() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool
        .setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), HotDeploymentTool.ACTION_DELETE).toFile());
    jonasHotDeploymentTool.setTask(task);

    // Act
    jonasHotDeploymentTool.validateAttributes();

    // Assert
    CommandlineJava commandLine = jonasHotDeploymentTool.getJava().getCommandLine();
    String[] commandline = commandLine.getCommandline();
    assertEquals("-r", commandline[1]);
    assertEquals("null", commandline[2]);
    Commandline vmCommand = commandLine.getVmCommand();
    assertEquals(0, vmCommand.getArguments().length);
    assertEquals(1, vmCommand.getCommandline().length);
    assertEquals(3, commandLine.size());
    assertEquals(3, commandline.length);
    assertFalse(vmCommand.iterator().hasNext());
  }

  /**
   * Test {@link JonasHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenThrowBuildException() throws BuildException {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link JonasHotDeploymentTool#isActionValid()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DELETE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#isActionValid()}
   */
  @Test
  public void testIsActionValid_givenServerDeployActionIsAction_delete_thenReturnTrue() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act and Assert
    assertTrue(jonasHotDeploymentTool.isActionValid());
  }

  /**
   * Test {@link JonasHotDeploymentTool#isActionValid()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@code Action}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasHotDeploymentTool#isActionValid()}
   */
  @Test
  public void testIsActionValid_givenServerDeployActionIsAction_thenReturnFalse() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction("Action");

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(task);

    // Act and Assert
    assertFalse(jonasHotDeploymentTool.isActionValid());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link JonasHotDeploymentTool}
   *   <li>{@link JonasHotDeploymentTool#setDavidhost(String)}
   *   <li>{@link JonasHotDeploymentTool#setDavidport(int)}
   *   <li>{@link JonasHotDeploymentTool#setJonasroot(File)}
   *   <li>{@link JonasHotDeploymentTool#setOrb(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    JonasHotDeploymentTool actualJonasHotDeploymentTool = new JonasHotDeploymentTool();
    actualJonasHotDeploymentTool.setDavidhost("42");
    actualJonasHotDeploymentTool.setDavidport(42);
    actualJonasHotDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualJonasHotDeploymentTool.setOrb("42");

    // Assert
    assertNull(actualJonasHotDeploymentTool.getPassword());
    assertNull(actualJonasHotDeploymentTool.getServer());
    assertNull(actualJonasHotDeploymentTool.getUserName());
    assertNull(actualJonasHotDeploymentTool.getClassName());
    assertNull(actualJonasHotDeploymentTool.getJava());
    assertNull(actualJonasHotDeploymentTool.getTask());
  }
}

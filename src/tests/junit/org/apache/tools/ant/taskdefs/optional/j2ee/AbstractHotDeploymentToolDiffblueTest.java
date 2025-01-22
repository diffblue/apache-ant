package org.apache.tools.ant.taskdefs.optional.j2ee;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Java;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class AbstractHotDeploymentToolDiffblueTest {
  /**
   * Test {@link AbstractHotDeploymentTool#createClasspath()}.
   * <ul>
   *   <li>Then {@link GenericHotDeploymentTool} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenGenericHotDeploymentToolClasspathDescriptionIsNull() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setTask(new ServerDeploy());

    // Act
    Path actualCreateClasspathResult = genericHotDeploymentTool.createClasspath();

    // Assert
    Path classpath = genericHotDeploymentTool.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#createClasspath()}.
   * <ul>
   *   <li>Then {@link GenericHotDeploymentTool} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenGenericHotDeploymentToolClasspathIsSystemBootClasspath() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = genericHotDeploymentTool.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, genericHotDeploymentTool.getClasspath());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DEPLOY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsAction_deploy() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> genericHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@code The "action" attribute must be set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsTheActionAttributeMustBeSet() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction("The \"action\" attribute must be set");

    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> genericHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenThrowBuildException() throws BuildException {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertThrows(BuildException.class, () -> genericHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#setTask(ServerDeploy)}.
   * <ul>
   *   <li>Then {@link GenericHotDeploymentTool} (default constructor) Java Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#setTask(ServerDeploy)}
   */
  @Test
  public void testSetTask_thenGenericHotDeploymentToolJavaDescriptionIsNull() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();

    // Act
    genericHotDeploymentTool.setTask(new ServerDeploy());

    // Assert
    Java java = genericHotDeploymentTool.getJava();
    assertNull(java.getDescription());
    assertNull(java.getTaskName());
    assertNull(java.getTaskType());
    assertNull(java.getProject());
    assertNull(java.getOwningTarget());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#setTask(ServerDeploy)}.
   * <ul>
   *   <li>Then {@link WebLogicHotDeploymentTool} (default constructor) Task is {@link ServerDeploy} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#setTask(ServerDeploy)}
   */
  @Test
  public void testSetTask_thenWebLogicHotDeploymentToolTaskIsServerDeploy() {
    // Arrange
    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    ServerDeploy task = new ServerDeploy();

    // Act
    webLogicHotDeploymentTool.setTask(task);

    // Assert
    assertSame(task, webLogicHotDeploymentTool.getTask());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getTask()}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getTask()}
   */
  @Test
  public void testGetTask() {
    // Arrange, Act and Assert
    assertNull((new GenericHotDeploymentTool()).getTask());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    Project project = new Project();
    task.setProject(project);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setTask(task);

    // Act and Assert
    assertSame(project, jonasHotDeploymentTool.getClasspath().getProject());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Given {@link GenericHotDeploymentTool} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenGenericHotDeploymentTool_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new GenericHotDeploymentTool()).getClasspath());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Given {@link JonasHotDeploymentTool} (default constructor) Orb is {@code 42}.</li>
   *   <li>Then return size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenJonasHotDeploymentToolOrbIs42_thenReturnSizeIsTwo() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setTask(new ServerDeploy());

    // Act
    Path actualClasspath = jonasHotDeploymentTool.getClasspath();

    // Assert
    Location location = actualClasspath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualClasspath.getDescription());
    assertNull(actualClasspath.getProject());
    assertNull(actualClasspath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(2, actualClasspath.size());
    assertFalse(actualClasspath.isReference());
    assertFalse(actualClasspath.isEmpty());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenServerDeployProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    Project project = new Project();
    task.setProject(project);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setTask(task);

    // Act and Assert
    assertSame(project, jonasHotDeploymentTool.getClasspath().getProject());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnProjectBuildListenersFirstIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    ServerDeploy task = new ServerDeploy();
    task.setProject(project);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setTask(task);

    // Act and Assert
    Vector<BuildListener> buildListeners = jonasHotDeploymentTool.getClasspath().getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("_jonas.jar", typeClass);
    project.addBuildListener(new AntClassLoader());

    ServerDeploy task = new ServerDeploy();
    task.setProject(project);

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setTask(task);

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
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnSizeIsZero() {
    // Arrange
    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setTask(new ServerDeploy());

    // Act
    Path actualClasspath = jonasHotDeploymentTool.getClasspath();

    // Assert
    Location location = actualClasspath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualClasspath.getDescription());
    assertNull(actualClasspath.getProject());
    assertNull(actualClasspath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualClasspath.size());
    assertFalse(actualClasspath.isReference());
    assertTrue(actualClasspath.isEmpty());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnSystemBootClasspath() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setProject(new Project());

    JonasHotDeploymentTool jonasHotDeploymentTool = new JonasHotDeploymentTool();
    jonasHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    jonasHotDeploymentTool.setOrb("42");
    jonasHotDeploymentTool.setTask(task);

    // Act
    Path actualClasspath = jonasHotDeploymentTool.getClasspath();

    // Assert
    Path path = actualClasspath.systemBootClasspath;
    assertSame(path, actualClasspath);
    assertSame(path, jonasHotDeploymentTool.getClasspath());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#setClasspath(Path)}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    Path classpath = Path.systemBootClasspath;

    // Act
    genericHotDeploymentTool.setClasspath(classpath);

    // Assert
    Path expectedClasspath = classpath.systemBootClasspath;
    assertSame(expectedClasspath, genericHotDeploymentTool.getClasspath());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getUserName()}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getUserName()}
   */
  @Test
  public void testGetUserName() {
    // Arrange, Act and Assert
    assertNull((new GenericHotDeploymentTool()).getUserName());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#setUserName(String)}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#setUserName(String)}
   */
  @Test
  public void testSetUserName() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();

    // Act
    genericHotDeploymentTool.setUserName("janedoe");

    // Assert
    assertEquals("janedoe", genericHotDeploymentTool.getUserName());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getPassword()}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getPassword()}
   */
  @Test
  public void testGetPassword() {
    // Arrange, Act and Assert
    assertNull((new GenericHotDeploymentTool()).getPassword());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#setPassword(String)}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#setPassword(String)}
   */
  @Test
  public void testSetPassword() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();

    // Act
    genericHotDeploymentTool.setPassword("iloveyou");

    // Assert
    assertEquals("iloveyou", genericHotDeploymentTool.getPassword());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#getServer()}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#getServer()}
   */
  @Test
  public void testGetServer() {
    // Arrange, Act and Assert
    assertNull((new GenericHotDeploymentTool()).getServer());
  }

  /**
   * Test {@link AbstractHotDeploymentTool#setServer(String)}.
   * <p>
   * Method under test: {@link AbstractHotDeploymentTool#setServer(String)}
   */
  @Test
  public void testSetServer() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();

    // Act
    genericHotDeploymentTool.setServer("Server");

    // Assert
    assertEquals("Server", genericHotDeploymentTool.getServer());
  }
}

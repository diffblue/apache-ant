package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.taskdefs.optional.ejb.EjbJar.CMPVersion;
import org.apache.tools.ant.taskdefs.optional.ejb.EjbJar.Config;
import org.apache.tools.ant.taskdefs.optional.ejb.EjbJar.DTDLocation;
import org.apache.tools.ant.taskdefs.optional.ejb.EjbJar.NamingScheme;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class EjbJarDiffblueTest {
  /**
   * Test {@link EjbJar#addDeploymentTool(EJBDeploymentTool)}.
   * <ul>
   *   <li>Then {@link BorlandDeploymentTool} (default constructor) Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#addDeploymentTool(EJBDeploymentTool)}
   */
  @Test
  public void testAddDeploymentTool_thenBorlandDeploymentToolLocationFileNameIsNull() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    BorlandDeploymentTool deploymentTool = new BorlandDeploymentTool();

    // Act
    ejbJar.addDeploymentTool(deploymentTool);

    // Assert
    Location location = deploymentTool.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, deploymentTool.getTask());
    assertSame(location, ejbJar.getLocation());
  }

  /**
   * Test CMPVersion {@link CMPVersion#getValues()}.
   * <p>
   * Method under test: {@link CMPVersion#getValues()}
   */
  @Test
  public void testCMPVersionGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{CMPVersion.CMP1_0, CMPVersion.CMP2_0}, (new CMPVersion()).getValues());
  }

  /**
   * Test CMPVersion new {@link CMPVersion} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CMPVersion}
   */
  @Test
  public void testCMPVersionNewCMPVersion() {
    // Arrange and Act
    CMPVersion actualCmpVersion = new CMPVersion();

    // Assert
    assertNull(actualCmpVersion.getValue());
    assertEquals(-1, actualCmpVersion.getIndex());
  }

  /**
   * Test Config new {@link Config} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Config}
   */
  @Test
  public void testConfigNewConfig() {
    // Arrange, Act and Assert
    assertTrue((new Config()).supportFileSets.isEmpty());
  }

  /**
   * Test {@link EjbJar#createOrion()}.
   * <p>
   * Method under test: {@link EjbJar#createOrion()}
   */
  @Test
  public void testCreateOrion() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    OrionDeploymentTool actualCreateOrionResult = ejbJar.createOrion();

    // Assert
    assertNull(actualCreateOrionResult.getDestDir());
    Location location = actualCreateOrionResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateOrionResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateOrionResult.getTask());
  }

  /**
   * Test {@link EjbJar#createWeblogic()}.
   * <p>
   * Method under test: {@link EjbJar#createWeblogic()}
   */
  @Test
  public void testCreateWeblogic() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    WeblogicDeploymentTool actualCreateWeblogicResult = ejbJar.createWeblogic();

    // Assert
    assertNull(actualCreateWeblogicResult.getDestDir());
    assertNull(actualCreateWeblogicResult.getJvmDebugLevel());
    Location location = actualCreateWeblogicResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateWeblogicResult.getEjbcClass());
    assertNull(actualCreateWeblogicResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateWeblogicResult.getTask());
  }

  /**
   * Test {@link EjbJar#createWebsphere()}.
   * <p>
   * Method under test: {@link EjbJar#createWebsphere()}
   */
  @Test
  public void testCreateWebsphere() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    WebsphereDeploymentTool actualCreateWebsphereResult = ejbJar.createWebsphere();

    // Assert
    assertEquals(" -quiet", actualCreateWebsphereResult.getOptions());
    assertNull(actualCreateWebsphereResult.getDestDir());
    Location location = actualCreateWebsphereResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateWebsphereResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateWebsphereResult.getTask());
  }

  /**
   * Test {@link EjbJar#createBorland()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with three.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createBorland()}
   */
  @Test
  public void testCreateBorland_givenByteArrayOutputStreamWithThree() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(3)));

    Project project = new Project();
    project.addBuildListener(listener);

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createBorland().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createBorland()}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor).</li>
   *   <li>Then return DestDir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createBorland()}
   */
  @Test
  public void testCreateBorland_givenEjbJar_thenReturnDestDirIsNull() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    BorlandDeploymentTool actualCreateBorlandResult = ejbJar.createBorland();

    // Assert
    assertNull(actualCreateBorlandResult.getDestDir());
    Location location = actualCreateBorlandResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateBorlandResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateBorlandResult.getTask());
  }

  /**
   * Test {@link EjbJar#createBorland()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createBorland()}
   */
  @Test
  public void testCreateBorland_thenReturnTaskProjectBuildListenersEmpty() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(new Project());

    // Act and Assert
    Task task = ejbJar.createBorland().getTask();
    assertTrue(task instanceof EjbJar);
    Project project = task.getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createBorland()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createBorland()}
   */
  @Test
  public void testCreateBorland_thenReturnTaskProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createBorland().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createBorland()}.
   * <ul>
   *   <li>Then return Task Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createBorland()}
   */
  @Test
  public void testCreateBorland_thenReturnTaskProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("-generic.jar", typeClass);
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createBorland().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(dataTypeDefinitions.containsKey("-generic.jar"));
    assertTrue(copyOfDataTypeDefinitions.containsKey("-generic.jar"));
  }

  /**
   * Test {@link EjbJar#createIplanet()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with three.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createIplanet()}
   */
  @Test
  public void testCreateIplanet_givenByteArrayOutputStreamWithThree() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(3)));

    Project project = new Project();
    project.addBuildListener(listener);

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createIplanet().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createIplanet()}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor).</li>
   *   <li>Then return DestDir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createIplanet()}
   */
  @Test
  public void testCreateIplanet_givenEjbJar_thenReturnDestDirIsNull() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    IPlanetDeploymentTool actualCreateIplanetResult = ejbJar.createIplanet();

    // Assert
    assertNull(actualCreateIplanetResult.getDestDir());
    Location location = actualCreateIplanetResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateIplanetResult.getPublicId());
    assertNull(actualCreateIplanetResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateIplanetResult.getTask());
  }

  /**
   * Test {@link EjbJar#createIplanet()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createIplanet()}
   */
  @Test
  public void testCreateIplanet_thenReturnTaskProjectBuildListenersEmpty() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(new Project());

    // Act and Assert
    Task task = ejbJar.createIplanet().getTask();
    assertTrue(task instanceof EjbJar);
    Project project = task.getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createIplanet()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createIplanet()}
   */
  @Test
  public void testCreateIplanet_thenReturnTaskProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createIplanet().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createIplanet()}.
   * <ul>
   *   <li>Then return Task Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createIplanet()}
   */
  @Test
  public void testCreateIplanet_thenReturnTaskProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("-generic.jar", typeClass);
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createIplanet().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(dataTypeDefinitions.containsKey("-generic.jar"));
    assertTrue(copyOfDataTypeDefinitions.containsKey("-generic.jar"));
  }

  /**
   * Test {@link EjbJar#createJboss()}.
   * <p>
   * Method under test: {@link EjbJar#createJboss()}
   */
  @Test
  public void testCreateJboss() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    JbossDeploymentTool actualCreateJbossResult = ejbJar.createJboss();

    // Assert
    assertNull(actualCreateJbossResult.getDestDir());
    Location location = actualCreateJbossResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateJbossResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateJbossResult.getTask());
  }

  /**
   * Test {@link EjbJar#createJonas()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with three.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createJonas()}
   */
  @Test
  public void testCreateJonas_givenByteArrayOutputStreamWithThree() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(3)));

    Project project = new Project();
    project.addBuildListener(listener);

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createJonas().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createJonas()}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor).</li>
   *   <li>Then return DestDir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createJonas()}
   */
  @Test
  public void testCreateJonas_givenEjbJar_thenReturnDestDirIsNull() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    JonasDeploymentTool actualCreateJonasResult = ejbJar.createJonas();

    // Assert
    assertNull(actualCreateJonasResult.getDestDir());
    Location location = actualCreateJonasResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateJonasResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateJonasResult.getTask());
  }

  /**
   * Test {@link EjbJar#createJonas()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createJonas()}
   */
  @Test
  public void testCreateJonas_thenReturnTaskProjectBuildListenersEmpty() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(new Project());

    // Act and Assert
    Task task = ejbJar.createJonas().getTask();
    assertTrue(task instanceof EjbJar);
    Project project = task.getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createJonas()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createJonas()}
   */
  @Test
  public void testCreateJonas_thenReturnTaskProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createJonas().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createJonas()}.
   * <ul>
   *   <li>Then return Task Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createJonas()}
   */
  @Test
  public void testCreateJonas_thenReturnTaskProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("-generic.jar", typeClass);
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createJonas().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(dataTypeDefinitions.containsKey("-generic.jar"));
    assertTrue(copyOfDataTypeDefinitions.containsKey("-generic.jar"));
  }

  /**
   * Test {@link EjbJar#createWeblogictoplink()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with two.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createWeblogictoplink()}
   */
  @Test
  public void testCreateWeblogictoplink_givenByteArrayOutputStreamWithTwo() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(2)));

    Project project = new Project();
    project.addBuildListener(listener);

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createWeblogictoplink().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createWeblogictoplink()}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor).</li>
   *   <li>Then return DestDir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createWeblogictoplink()}
   */
  @Test
  public void testCreateWeblogictoplink_givenEjbJar_thenReturnDestDirIsNull() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    WeblogicTOPLinkDeploymentTool actualCreateWeblogictoplinkResult = ejbJar.createWeblogictoplink();

    // Assert
    assertNull(actualCreateWeblogictoplinkResult.getDestDir());
    assertNull(actualCreateWeblogictoplinkResult.getJvmDebugLevel());
    Location location = actualCreateWeblogictoplinkResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateWeblogictoplinkResult.getEjbcClass());
    assertNull(actualCreateWeblogictoplinkResult.getConfig());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(ejbJar, actualCreateWeblogictoplinkResult.getTask());
  }

  /**
   * Test {@link EjbJar#createWeblogictoplink()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createWeblogictoplink()}
   */
  @Test
  public void testCreateWeblogictoplink_thenReturnTaskProjectBuildListenersEmpty() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(new Project());

    // Act and Assert
    Task task = ejbJar.createWeblogictoplink().getTask();
    assertTrue(task instanceof EjbJar);
    Project project = task.getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createWeblogictoplink()}.
   * <ul>
   *   <li>Then return Task Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createWeblogictoplink()}
   */
  @Test
  public void testCreateWeblogictoplink_thenReturnTaskProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createWeblogictoplink().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    assertEquals(1, project2.getBuildListeners().size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, task.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link EjbJar#createWeblogictoplink()}.
   * <ul>
   *   <li>Then return Task Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createWeblogictoplink()}
   */
  @Test
  public void testCreateWeblogictoplink_thenReturnTaskProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("-generic.jar", typeClass);
    project.addBuildListener(new AntClassLoader());

    EjbJar ejbJar = new EjbJar();
    ejbJar.setProject(project);

    // Act and Assert
    Task task = ejbJar.createWeblogictoplink().getTask();
    assertTrue(task instanceof EjbJar);
    Project project2 = task.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(dataTypeDefinitions.containsKey("-generic.jar"));
    assertTrue(copyOfDataTypeDefinitions.containsKey("-generic.jar"));
  }

  /**
   * Test {@link EjbJar#createClasspath()}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenEjbJarProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    Project project = new Project();
    ejbJar.setProject(project);

    // Act and Assert
    assertSame(project, ejbJar.createClasspath().getProject());
  }

  /**
   * Test {@link EjbJar#createClasspath()}.
   * <ul>
   *   <li>Given {@link EjbJar} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenEjbJar_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new EjbJar()).createClasspath();

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
   * Test {@link EjbJar#createDTD()}.
   * <p>
   * Method under test: {@link EjbJar#createDTD()}
   */
  @Test
  public void testCreateDTD() {
    // Arrange and Act
    DTDLocation actualCreateDTDResult = (new EjbJar()).createDTD();

    // Assert
    assertNull(actualCreateDTDResult.getLocation());
    assertNull(actualCreateDTDResult.getPublicId());
    assertNull(actualCreateDTDResult.getBase());
  }

  /**
   * Test {@link EjbJar#createSupport()}.
   * <p>
   * Method under test: {@link EjbJar#createSupport()}
   */
  @Test
  public void testCreateSupport() {
    // Arrange and Act
    FileSet actualCreateSupportResult = (new EjbJar()).createSupport();

    // Assert
    assertNull(actualCreateSupportResult.getDir());
    Location location = actualCreateSupportResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateSupportResult.getDescription());
    assertNull(actualCreateSupportResult.getProject());
    assertNull(actualCreateSupportResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualCreateSupportResult.getMaxLevelsOfSymlinks());
    assertFalse(actualCreateSupportResult.isReference());
    assertTrue(actualCreateSupportResult.getDefaultexcludes());
    assertTrue(actualCreateSupportResult.getErrorOnMissingDir());
    assertTrue(actualCreateSupportResult.isFilesystemOnly());
  }

  /**
   * Test DTDLocation new {@link DTDLocation} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DTDLocation}
   */
  @Test
  public void testDTDLocationNewDTDLocation() {
    // Arrange and Act
    DTDLocation actualDtdLocation = new DTDLocation();

    // Assert
    assertNull(actualDtdLocation.getLocation());
    assertNull(actualDtdLocation.getPublicId());
    assertNull(actualDtdLocation.getBase());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link EjbJar#setDestdir(File)}
   *   <li>{@link EjbJar#setGenericjarsuffix(String)}
   *   <li>{@link EjbJar#setBasenameterminator(String)}
   *   <li>{@link EjbJar#setClasspath(Path)}
   *   <li>{@link EjbJar#setDependency(String)}
   *   <li>{@link EjbJar#setDescriptordir(File)}
   *   <li>{@link EjbJar#setFlatdestdir(boolean)}
   *   <li>{@link EjbJar#setManifest(File)}
   *   <li>{@link EjbJar#setSrcdir(File)}
   *   <li>{@link EjbJar#getCmpversion()}
   *   <li>{@link EjbJar#getDestdir()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    EjbJar ejbJar = new EjbJar();
    File inDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    ejbJar.setDestdir(inDir);
    ejbJar.setGenericjarsuffix("In String");
    ejbJar.setBasenameterminator("42");
    ejbJar.setClasspath(Path.systemBootClasspath);
    ejbJar.setDependency("Analyzer");
    ejbJar.setDescriptordir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    ejbJar.setFlatdestdir(true);
    ejbJar.setManifest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    ejbJar.setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    String actualCmpversion = ejbJar.getCmpversion();

    // Assert
    assertEquals(CMPVersion.CMP1_0, actualCmpversion);
    assertSame(inDir, ejbJar.getDestdir());
  }

  /**
   * Test NamingScheme {@link NamingScheme#getValues()}.
   * <p>
   * Method under test: {@link NamingScheme#getValues()}
   */
  @Test
  public void testNamingSchemeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(
        new String[]{NamingScheme.EJB_NAME, NamingScheme.DIRECTORY, NamingScheme.DESCRIPTOR, NamingScheme.BASEJARNAME},
        (new NamingScheme()).getValues());
  }

  /**
   * Test NamingScheme new {@link NamingScheme} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link NamingScheme}
   */
  @Test
  public void testNamingSchemeNewNamingScheme() {
    // Arrange and Act
    NamingScheme actualNamingScheme = new NamingScheme();

    // Assert
    assertNull(actualNamingScheme.getValue());
    assertEquals(-1, actualNamingScheme.getIndex());
  }

  /**
   * Test {@link EjbJar#setCmpversion(CMPVersion)}.
   * <ul>
   *   <li>When {@link CMPVersion} (default constructor).</li>
   *   <li>Then {@link EjbJar} (default constructor) Cmpversion is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EjbJar#setCmpversion(CMPVersion)}
   */
  @Test
  public void testSetCmpversion_whenCMPVersion_thenEjbJarCmpversionIsNull() {
    // Arrange
    EjbJar ejbJar = new EjbJar();

    // Act
    ejbJar.setCmpversion(new CMPVersion());

    // Assert
    assertNull(ejbJar.getCmpversion());
  }

  /**
   * Test {@link EjbJar#execute()}.
   * <p>
   * Method under test: {@link EjbJar#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new EjbJar()).execute());
  }

  /**
   * Test new {@link EjbJar} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link EjbJar}
   */
  @Test
  public void testNewEjbJar() {
    // Arrange and Act
    EjbJar actualEjbJar = new EjbJar();

    // Assert
    assertNull(actualEjbJar.getDestdir());
    assertNull(actualEjbJar.getDescription());
    assertNull(actualEjbJar.getTaskName());
    assertNull(actualEjbJar.getTaskType());
    assertNull(actualEjbJar.getProject());
    assertNull(actualEjbJar.getOwningTarget());
    assertFalse(actualEjbJar.hasSelectors());
    assertEquals(CMPVersion.CMP1_0, actualEjbJar.getCmpversion());
  }
}

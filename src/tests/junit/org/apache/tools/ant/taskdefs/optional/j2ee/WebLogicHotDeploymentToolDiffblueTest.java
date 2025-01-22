package org.apache.tools.ant.taskdefs.optional.j2ee;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class WebLogicHotDeploymentToolDiffblueTest {
  /**
   * Test {@link WebLogicHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DELETE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsAction_delete() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);
    task.setSource(null);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    webLogicHotDeploymentTool.setPassword("iloveyou");
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setApplication(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> webLogicHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_UPDATE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenServerDeployActionIsAction_update() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_UPDATE);
    task.setSource(null);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    webLogicHotDeploymentTool.setPassword("iloveyou");
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setApplication(HotDeploymentTool.ACTION_DELETE);

    // Act and Assert
    assertThrows(BuildException.class, () -> webLogicHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link WebLogicHotDeploymentTool} (default constructor) Application is {@link HotDeploymentTool#ACTION_DEPLOY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenWebLogicHotDeploymentToolApplicationIsAction_deploy() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);
    task.setSource(null);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    webLogicHotDeploymentTool.setPassword("iloveyou");
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setApplication(HotDeploymentTool.ACTION_DEPLOY);

    // Act and Assert
    assertThrows(BuildException.class, () -> webLogicHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link WebLogicHotDeploymentTool} (default constructor) Application is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenWebLogicHotDeploymentToolApplicationIsNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);
    task.setSource(null);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    webLogicHotDeploymentTool.setPassword("iloveyou");
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setApplication(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> webLogicHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Given {@link WebLogicHotDeploymentTool} (default constructor) Password is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenWebLogicHotDeploymentToolPasswordIsNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);
    task.setSource(null);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    webLogicHotDeploymentTool.setPassword(null);
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setApplication(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> webLogicHotDeploymentTool.validateAttributes());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DELETE}.</li>
   *   <li>Then return {@code delete null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_givenServerDeployActionIsAction_delete_thenReturnDeleteNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("  delete null null ", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_LIST}.</li>
   *   <li>Then return {@code list null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_givenServerDeployActionIsAction_list_thenReturnListNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_LIST);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("  list null ", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@code Action}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_givenServerDeployActionIsAction_thenReturnNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction("Action");
    task.addGeneric(new GenericHotDeploymentTool());

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);

    // Act and Assert
    assertNull(webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code -component foo deploy null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnComponentFooDeployNullNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent("foo");
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("-component foo   deploy null null null", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code -debug deploy null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnDebugDeployNullNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(true);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals(" -debug  deploy null null null", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code deploy null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnDeployNullNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("  deploy null null null", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code undeploy null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnUndeployNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_UNDEPLOY);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("  undeploy null null ", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code update null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnUpdateNullNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_UPDATE);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("  update null null null", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code -url foo deploy null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnUrlFooDeployNullNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer("foo");
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName(null);

    // Act and Assert
    assertEquals("-url foo  deploy null null null", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#getArguments()}.
   * <ul>
   *   <li>Then return {@code -username foo deploy null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#getArguments()}
   */
  @Test
  public void testGetArguments_thenReturnUsernameFooDeployNullNullNull() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);
    webLogicHotDeploymentTool.setComponent(null);
    webLogicHotDeploymentTool.setServer(null);
    webLogicHotDeploymentTool.setDebug(false);
    webLogicHotDeploymentTool.setUserName("foo");

    // Act and Assert
    assertEquals(" -username foo deploy null null null", webLogicHotDeploymentTool.getArguments());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#isActionValid()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DELETE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#isActionValid()}
   */
  @Test
  public void testIsActionValid_givenServerDeployActionIsAction_delete_thenReturnTrue() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DELETE);

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);

    // Act and Assert
    assertTrue(webLogicHotDeploymentTool.isActionValid());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#isActionValid()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@code Action}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#isActionValid()}
   */
  @Test
  public void testIsActionValid_givenServerDeployActionIsAction_thenReturnFalse() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction("Action");

    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(task);

    // Act and Assert
    assertFalse(webLogicHotDeploymentTool.isActionValid());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#buildArgsPrefix()}.
   * <ul>
   *   <li>Then return toString is {@code null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#buildArgsPrefix()}
   */
  @Test
  public void testBuildArgsPrefix_thenReturnToStringIsNullNull() {
    // Arrange
    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertEquals("  null null ", webLogicHotDeploymentTool.buildArgsPrefix().toString());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#buildDeployArgs()}.
   * <ul>
   *   <li>Then return {@code -component null null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#buildDeployArgs()}
   */
  @Test
  public void testBuildDeployArgs_thenReturnComponentNullNullNullNull() {
    // Arrange
    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setComponent(" ");
    webLogicHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertEquals("-component     null null null null", webLogicHotDeploymentTool.buildDeployArgs());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#buildDeployArgs()}.
   * <ul>
   *   <li>Then return {@code null null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#buildDeployArgs()}
   */
  @Test
  public void testBuildDeployArgs_thenReturnNullNullNullNull() {
    // Arrange
    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertEquals("  null null null null", webLogicHotDeploymentTool.buildDeployArgs());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#buildUndeployArgs()}.
   * <ul>
   *   <li>Then return {@code null null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#buildUndeployArgs()}
   */
  @Test
  public void testBuildUndeployArgs_thenReturnNullNullNull() {
    // Arrange
    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertEquals("  null null null ", webLogicHotDeploymentTool.buildUndeployArgs());
  }

  /**
   * Test {@link WebLogicHotDeploymentTool#buildListArgs()}.
   * <ul>
   *   <li>Then return {@code null null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WebLogicHotDeploymentTool#buildListArgs()}
   */
  @Test
  public void testBuildListArgs_thenReturnNullNull() {
    // Arrange
    WebLogicHotDeploymentTool webLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    webLogicHotDeploymentTool.setTask(new ServerDeploy());

    // Act and Assert
    assertEquals("  null null ", webLogicHotDeploymentTool.buildListArgs());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link WebLogicHotDeploymentTool}
   *   <li>{@link WebLogicHotDeploymentTool#setApplication(String)}
   *   <li>{@link WebLogicHotDeploymentTool#setComponent(String)}
   *   <li>{@link WebLogicHotDeploymentTool#setDebug(boolean)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    WebLogicHotDeploymentTool actualWebLogicHotDeploymentTool = new WebLogicHotDeploymentTool();
    actualWebLogicHotDeploymentTool.setApplication("Application");
    actualWebLogicHotDeploymentTool.setComponent("Component");
    actualWebLogicHotDeploymentTool.setDebug(true);

    // Assert
    assertNull(actualWebLogicHotDeploymentTool.getPassword());
    assertNull(actualWebLogicHotDeploymentTool.getServer());
    assertNull(actualWebLogicHotDeploymentTool.getUserName());
    assertNull(actualWebLogicHotDeploymentTool.getTask());
    assertNull(actualWebLogicHotDeploymentTool.getClasspath());
  }
}

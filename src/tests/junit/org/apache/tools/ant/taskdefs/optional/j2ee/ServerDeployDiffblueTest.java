package org.apache.tools.ant.taskdefs.optional.j2ee;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Java;
import org.junit.Test;

public class ServerDeployDiffblueTest {
  /**
   * Test {@link ServerDeploy#addGeneric(GenericHotDeploymentTool)}.
   * <ul>
   *   <li>Then {@link GenericHotDeploymentTool} (default constructor) Java Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ServerDeploy#addGeneric(GenericHotDeploymentTool)}
   */
  @Test
  public void testAddGeneric_thenGenericHotDeploymentToolJavaDescriptionIsNull() {
    // Arrange
    ServerDeploy serverDeploy = new ServerDeploy();
    GenericHotDeploymentTool tool = new GenericHotDeploymentTool();

    // Act
    serverDeploy.addGeneric(tool);

    // Assert
    Java java = tool.getJava();
    assertNull(java.getDescription());
    assertNull(java.getTaskName());
    assertNull(java.getTaskType());
    assertNull(java.getProject());
    assertNull(java.getOwningTarget());
    assertSame(serverDeploy, tool.getTask());
  }

  /**
   * Test {@link ServerDeploy#addWeblogic(WebLogicHotDeploymentTool)}.
   * <ul>
   *   <li>Then {@link WebLogicHotDeploymentTool} (default constructor) Task is {@link ServerDeploy} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ServerDeploy#addWeblogic(WebLogicHotDeploymentTool)}
   */
  @Test
  public void testAddWeblogic_thenWebLogicHotDeploymentToolTaskIsServerDeploy() {
    // Arrange
    ServerDeploy serverDeploy = new ServerDeploy();
    WebLogicHotDeploymentTool tool = new WebLogicHotDeploymentTool();

    // Act
    serverDeploy.addWeblogic(tool);

    // Assert
    assertSame(serverDeploy, tool.getTask());
  }

  /**
   * Test {@link ServerDeploy#addJonas(JonasHotDeploymentTool)}.
   * <ul>
   *   <li>Then {@link JonasHotDeploymentTool} (default constructor) Task is {@link ServerDeploy} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ServerDeploy#addJonas(JonasHotDeploymentTool)}
   */
  @Test
  public void testAddJonas_thenJonasHotDeploymentToolTaskIsServerDeploy() {
    // Arrange
    ServerDeploy serverDeploy = new ServerDeploy();
    JonasHotDeploymentTool tool = new JonasHotDeploymentTool();

    // Act
    serverDeploy.addJonas(tool);

    // Assert
    assertSame(serverDeploy, tool.getTask());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ServerDeploy#setAction(String)}
   *   <li>{@link ServerDeploy#setSource(File)}
   *   <li>{@link ServerDeploy#getAction()}
   *   <li>{@link ServerDeploy#getSource()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ServerDeploy serverDeploy = new ServerDeploy();

    // Act
    serverDeploy.setAction("Action");
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    serverDeploy.setSource(source);
    String actualAction = serverDeploy.getAction();

    // Assert
    assertEquals("Action", actualAction);
    assertSame(source, serverDeploy.getSource());
  }

  /**
   * Test new {@link ServerDeploy} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ServerDeploy}
   */
  @Test
  public void testNewServerDeploy() {
    // Arrange and Act
    ServerDeploy actualServerDeploy = new ServerDeploy();

    // Assert
    assertNull(actualServerDeploy.getSource());
    Location location = actualServerDeploy.getLocation();
    assertNull(location.getFileName());
    assertNull(actualServerDeploy.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualServerDeploy.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualServerDeploy.getTaskName());
    assertNull(actualServerDeploy.getTaskType());
    assertNull(actualServerDeploy.getAction());
    assertNull(actualServerDeploy.getProject());
    assertNull(actualServerDeploy.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualServerDeploy, runtimeConfigurableWrapper.getProxy());
  }
}

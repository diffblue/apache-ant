package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Input.Handler;
import org.apache.tools.ant.taskdefs.Input.HandlerType;
import org.junit.Test;

public class InputDiffblueTest {
  /**
   * Test {@link Input#createHandler()}.
   * <p>
   * Method under test: {@link Input#createHandler()}
   */
  @Test
  public void testCreateHandler() {
    // Arrange and Act
    Handler actualCreateHandlerResult = (new Input()).createHandler();

    // Assert
    assertEquals("", actualCreateHandlerResult.getURI());
    assertNull(actualCreateHandlerResult.getAntlibClassLoader());
    assertNull(actualCreateHandlerResult.getDescription());
    assertNull(actualCreateHandlerResult.getTaskName());
    assertNull(actualCreateHandlerResult.getTaskType());
    assertNull(actualCreateHandlerResult.getClasspathId());
    assertNull(actualCreateHandlerResult.getLoaderId());
    assertNull(actualCreateHandlerResult.getClassname());
    assertNull(actualCreateHandlerResult.getRefid());
    assertNull(actualCreateHandlerResult.getProject());
    assertNull(actualCreateHandlerResult.getOwningTarget());
    assertNull(actualCreateHandlerResult.getType());
    assertNull(actualCreateHandlerResult.getClasspath());
    assertFalse(actualCreateHandlerResult.isReverseLoader());
    assertTrue(actualCreateHandlerResult.hasCpDelegate());
  }

  /**
   * Test Handler getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Handler#Handler(Input)}
   *   <li>{@link Handler#setClassname(String)}
   *   <li>{@link Handler#setRefid(String)}
   *   <li>{@link Handler#setType(HandlerType)}
   *   <li>{@link Handler#getClassname()}
   *   <li>{@link Handler#getRefid()}
   *   <li>{@link Handler#getType()}
   * </ul>
   */
  @Test
  public void testHandlerGettersAndSetters() {
    // Arrange and Act
    Handler actualHandler = (new Input()).new Handler();
    actualHandler.setClassname("Classname");
    actualHandler.setRefid("Refid");
    HandlerType type = new HandlerType();
    actualHandler.setType(type);
    String actualClassname = actualHandler.getClassname();
    String actualRefid = actualHandler.getRefid();
    HandlerType actualType = actualHandler.getType();

    // Assert
    assertEquals("", actualHandler.getURI());
    assertEquals("Classname", actualClassname);
    assertEquals("Refid", actualRefid);
    assertNull(actualHandler.getAntlibClassLoader());
    Location location = actualHandler.getLocation();
    assertNull(location.getFileName());
    assertNull(actualHandler.getDescription());
    assertNull(actualHandler.getTaskName());
    assertNull(actualHandler.getTaskType());
    assertNull(actualHandler.getProject());
    assertNull(actualHandler.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(type, actualType);
  }

  /**
   * Test HandlerType getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link HandlerType}
   *   <li>{@link HandlerType#getValues()}
   * </ul>
   */
  @Test
  public void testHandlerTypeGettersAndSetters() {
    // Arrange and Act
    HandlerType actualHandlerType = new HandlerType();
    String[] actualValues = actualHandlerType.getValues();

    // Assert
    assertNull(actualHandlerType.getValue());
    assertEquals(-1, actualHandlerType.getIndex());
    assertArrayEquals(new String[]{"default", "propertyfile", "greedy", "secure"}, actualValues);
  }

  /**
   * Test new {@link Input} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Input}
   */
  @Test
  public void testNewInput() {
    // Arrange and Act
    Input actualInput = new Input();

    // Assert
    Location location = actualInput.getLocation();
    assertNull(location.getFileName());
    assertNull(actualInput.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualInput.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualInput.getTaskName());
    assertNull(actualInput.getTaskType());
    assertNull(actualInput.getProject());
    assertNull(actualInput.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualInput, runtimeConfigurableWrapper.getProxy());
  }
}

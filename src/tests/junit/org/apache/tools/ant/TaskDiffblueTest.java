package org.apache.tools.ant;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import org.junit.Test;

public class TaskDiffblueTest {
  /**
   * Test {@link Task#setOwningTarget(Target)}.
   * <p>
   * Method under test: {@link Task#setOwningTarget(Target)}
   */
  @Test
  public void testSetOwningTarget() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();
    Target target = new Target();

    // Act
    taskAdapter.setOwningTarget(target);

    // Assert
    assertSame(target, taskAdapter.getOwningTarget());
  }

  /**
   * Test {@link Task#getOwningTarget()}.
   * <p>
   * Method under test: {@link Task#getOwningTarget()}
   */
  @Test
  public void testGetOwningTarget() {
    // Arrange, Act and Assert
    assertNull((new TaskAdapter()).getOwningTarget());
  }

  /**
   * Test {@link Task#setTaskName(String)}.
   * <p>
   * Method under test: {@link Task#setTaskName(String)}
   */
  @Test
  public void testSetTaskName() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();

    // Act
    taskAdapter.setTaskName("Name");

    // Assert
    assertEquals("Name", taskAdapter.getRuntimeConfigurableWrapper().getElementTag());
    assertEquals("Name", taskAdapter.getTaskName());
  }

  /**
   * Test {@link Task#getTaskName()}.
   * <p>
   * Method under test: {@link Task#getTaskName()}
   */
  @Test
  public void testGetTaskName() {
    // Arrange, Act and Assert
    assertNull((new TaskAdapter()).getTaskName());
  }

  /**
   * Test {@link Task#setTaskType(String)}.
   * <p>
   * Method under test: {@link Task#setTaskType(String)}
   */
  @Test
  public void testSetTaskType() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();

    // Act
    taskAdapter.setTaskType("Type");

    // Assert
    assertEquals("Type", taskAdapter.getTaskType());
  }

  /**
   * Test {@link Task#getRuntimeConfigurableWrapper()}.
   * <p>
   * Method under test: {@link Task#getRuntimeConfigurableWrapper()}
   */
  @Test
  public void testGetRuntimeConfigurableWrapper() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();
    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");

    taskAdapter.setRuntimeConfigurableWrapper(wrapper);

    // Act and Assert
    assertSame(wrapper, taskAdapter.getRuntimeConfigurableWrapper());
  }

  /**
   * Test {@link Task#getRuntimeConfigurableWrapper()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return ElementTag is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Task#getRuntimeConfigurableWrapper()}
   */
  @Test
  public void testGetRuntimeConfigurableWrapper_givenTaskAdapter_thenReturnElementTagIsNull() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();

    // Act
    RuntimeConfigurable actualRuntimeConfigurableWrapper = taskAdapter.getRuntimeConfigurableWrapper();

    // Assert
    assertNull(actualRuntimeConfigurableWrapper.getElementTag());
    assertSame(taskAdapter, actualRuntimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link Task#setRuntimeConfigurableWrapper(RuntimeConfigurable)}.
   * <p>
   * Method under test: {@link Task#setRuntimeConfigurableWrapper(RuntimeConfigurable)}
   */
  @Test
  public void testSetRuntimeConfigurableWrapper() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();
    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");

    // Act
    taskAdapter.setRuntimeConfigurableWrapper(wrapper);

    // Assert
    assertSame(wrapper, taskAdapter.getRuntimeConfigurableWrapper());
    assertSame(wrapper, taskAdapter.getWrapper());
  }

  /**
   * Test {@link Task#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Task#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    DummyTaskOk dummyTaskOk = new DummyTaskOk();
    dummyTaskOk.setProject(project);

    // Act and Assert
    assertEquals(3, dummyTaskOk.handleInput(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, dummyTaskOk.getProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Task#perform()}.
   * <ul>
   *   <li>Then {@link TaskAdapter#TaskAdapter()} Proxy {@link UnknownElement}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Task#perform()}
   */
  @Test
  public void testPerform_thenTaskAdapterProxyUnknownElement() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProxy(new UnknownElement("execute"));
    taskAdapter.setProject(project);

    // Act
    taskAdapter.perform();

    // Assert
    Object proxy = taskAdapter.getProxy();
    assertTrue(proxy instanceof UnknownElement);
    assertSame(project, ((UnknownElement) proxy).getProject());
  }

  /**
   * Test {@link Task#markInvalid()}.
   * <p>
   * Method under test: {@link Task#markInvalid()}
   */
  @Test
  public void testMarkInvalid() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();

    // Act
    taskAdapter.markInvalid();

    // Assert
    assertTrue(taskAdapter.isInvalid());
  }

  /**
   * Test {@link Task#isInvalid()}.
   * <p>
   * Method under test: {@link Task#isInvalid()}
   */
  @Test
  public void testIsInvalid() {
    // Arrange, Act and Assert
    assertFalse((new TaskAdapter()).isInvalid());
  }

  /**
   * Test {@link Task#getTaskType()}.
   * <p>
   * Method under test: {@link Task#getTaskType()}
   */
  @Test
  public void testGetTaskType() {
    // Arrange, Act and Assert
    assertNull((new TaskAdapter()).getTaskType());
  }

  /**
   * Test {@link Task#getWrapper()}.
   * <p>
   * Method under test: {@link Task#getWrapper()}
   */
  @Test
  public void testGetWrapper() {
    // Arrange, Act and Assert
    assertNull((new TaskAdapter()).getWrapper());
  }
}

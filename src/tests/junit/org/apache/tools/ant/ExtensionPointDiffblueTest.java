package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.junit.Test;

public class ExtensionPointDiffblueTest {
  /**
   * Test {@link ExtensionPoint#ExtensionPoint()}.
   * <p>
   * Method under test: {@link ExtensionPoint#ExtensionPoint()}
   */
  @Test
  public void testNewExtensionPoint() {
    // Arrange and Act
    ExtensionPoint actualExtensionPoint = new ExtensionPoint();

    // Assert
    Location location = actualExtensionPoint.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExtensionPoint.getDescription());
    assertNull(actualExtensionPoint.getIf());
    assertNull(actualExtensionPoint.getName());
    assertNull(actualExtensionPoint.getUnless());
    assertNull(actualExtensionPoint.toString());
    assertNull(actualExtensionPoint.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualExtensionPoint.getTasks().length);
  }

  /**
   * Test {@link ExtensionPoint#ExtensionPoint(Target)}.
   * <ul>
   *   <li>When {@link Target#Target()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionPoint#ExtensionPoint(Target)}
   */
  @Test
  public void testNewExtensionPoint_whenTarget_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    ExtensionPoint actualExtensionPoint = new ExtensionPoint(new Target());

    // Assert
    Location location = actualExtensionPoint.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExtensionPoint.getDescription());
    assertNull(actualExtensionPoint.getIf());
    assertNull(actualExtensionPoint.getName());
    assertNull(actualExtensionPoint.getUnless());
    assertNull(actualExtensionPoint.toString());
    assertNull(actualExtensionPoint.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualExtensionPoint.getTasks().length);
  }

  /**
   * Test {@link ExtensionPoint#addTask(Task)}.
   * <p>
   * Method under test: {@link ExtensionPoint#addTask(Task)}
   */
  @Test
  public void testAddTask() {
    // Arrange
    ExtensionPoint extensionPoint = new ExtensionPoint();

    // Act and Assert
    assertThrows(BuildException.class, () -> extensionPoint.addTask(new TaskAdapter()));
  }

  /**
   * Test {@link ExtensionPoint#addDataType(RuntimeConfigurable)}.
   * <p>
   * Method under test: {@link ExtensionPoint#addDataType(RuntimeConfigurable)}
   */
  @Test
  public void testAddDataType() {
    // Arrange
    ExtensionPoint extensionPoint = new ExtensionPoint();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> extensionPoint.addDataType(new RuntimeConfigurable("Proxy", "Element Tag")));
  }
}

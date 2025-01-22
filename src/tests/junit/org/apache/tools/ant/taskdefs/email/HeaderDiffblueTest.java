package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class HeaderDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Header}
   *   <li>{@link Header#setName(String)}
   *   <li>{@link Header#setValue(String)}
   *   <li>{@link Header#getName()}
   *   <li>{@link Header#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Header actualHeader = new Header();
    actualHeader.setName("Name");
    actualHeader.setValue("42");
    String actualName = actualHeader.getName();

    // Assert
    assertEquals("42", actualHeader.getValue());
    assertEquals("Name", actualName);
  }
}

package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class Native2AsciiFilterDiffblueTest {
  /**
   * Test {@link Native2AsciiFilter#filter(String)}.
   * <ul>
   *   <li>Given {@link Native2AsciiFilter} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2AsciiFilter#filter(String)}
   */
  @Test
  public void testFilter_givenNative2AsciiFilter() {
    // Arrange, Act and Assert
    assertEquals("Line", (new Native2AsciiFilter()).filter("Line"));
  }

  /**
   * Test {@link Native2AsciiFilter#filter(String)}.
   * <ul>
   *   <li>Given {@link Native2AsciiFilter} (default constructor) Reverse is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Native2AsciiFilter#filter(String)}
   */
  @Test
  public void testFilter_givenNative2AsciiFilterReverseIsTrue() {
    // Arrange
    Native2AsciiFilter native2AsciiFilter = new Native2AsciiFilter();
    native2AsciiFilter.setReverse(true);

    // Act and Assert
    assertEquals("Line", native2AsciiFilter.filter("Line"));
  }

  /**
   * Test new {@link Native2AsciiFilter} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Native2AsciiFilter}
   */
  @Test
  public void testNewNative2AsciiFilter() {
    // Arrange and Act
    Native2AsciiFilter actualNative2AsciiFilter = new Native2AsciiFilter();

    // Assert
    Location location = actualNative2AsciiFilter.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNative2AsciiFilter.getDescription());
    assertNull(actualNative2AsciiFilter.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}

package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class UniqFilterDiffblueTest {
  /**
   * Test {@link UniqFilter#filter(String)}.
   * <p>
   * Method under test: {@link UniqFilter#filter(String)}
   */
  @Test
  public void testFilter() {
    // Arrange, Act and Assert
    assertEquals("String", (new UniqFilter()).filter("String"));
  }

  /**
   * Test new {@link UniqFilter} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link UniqFilter}
   */
  @Test
  public void testNewUniqFilter() {
    // Arrange and Act
    UniqFilter actualUniqFilter = new UniqFilter();

    // Assert
    Location location = actualUniqFilter.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUniqFilter.getDescription());
    assertNull(actualUniqFilter.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}

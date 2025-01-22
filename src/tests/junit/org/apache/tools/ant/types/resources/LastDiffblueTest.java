package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class LastDiffblueTest {
  /**
   * Test {@link Last#getCollection()}.
   * <ul>
   *   <li>Given {@link Last} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Last#getCollection()}
   */
  @Test
  public void testGetCollection_givenLastAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    Last last = new Last();
    last.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = last.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test new {@link Last} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Last}
   */
  @Test
  public void testNewLast() {
    // Arrange and Act
    Last actualLast = new Last();

    // Assert
    Location location = actualLast.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLast.getDescription());
    assertNull(actualLast.getProject());
    assertNull(actualLast.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1, actualLast.getCount());
    assertTrue(actualLast.isCache());
  }
}

package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.comparators.Content;
import org.junit.Test;

public class SortDiffblueTest {
  /**
   * Test {@link Sort#getCollection()}.
   * <ul>
   *   <li>Given {@link Sort} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Sort#getCollection()}
   */
  @Test
  public void testGetCollection_givenSortAddNone_thenReturnList() throws BuildException {
    // Arrange
    Sort sort = new Sort();
    sort.add(Resources.NONE);
    sort.add(new Content());

    // Act
    Collection<Resource> actualCollection = sort.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test new {@link Sort} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Sort}
   */
  @Test
  public void testNewSort() {
    // Arrange and Act
    Sort actualSort = new Sort();

    // Assert
    Location location = actualSort.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSort.getDescription());
    assertNull(actualSort.getProject());
    assertNull(actualSort.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSort.isReference());
    assertTrue(actualSort.isCache());
  }
}

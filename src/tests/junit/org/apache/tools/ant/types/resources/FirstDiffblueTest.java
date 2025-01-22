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

public class FirstDiffblueTest {
  /**
   * Test {@link First#getCollection()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link First#getCollection()}
   */
  @Test
  public void testGetCollection_givenFirstAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    First first = new First();
    first.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = first.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test new {@link First} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link First}
   */
  @Test
  public void testNewFirst() {
    // Arrange and Act
    First actualFirst = new First();

    // Assert
    Location location = actualFirst.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFirst.getDescription());
    assertNull(actualFirst.getProject());
    assertNull(actualFirst.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1, actualFirst.getCount());
    assertTrue(actualFirst.isCache());
  }
}

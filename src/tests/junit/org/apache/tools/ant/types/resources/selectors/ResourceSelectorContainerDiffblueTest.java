package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import java.util.List;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ResourceSelectorContainerDiffblueTest {
  /**
   * Test {@link ResourceSelectorContainer#ResourceSelectorContainer()}.
   * <p>
   * Method under test: {@link ResourceSelectorContainer#ResourceSelectorContainer()}
   */
  @Test
  public void testNewResourceSelectorContainer() {
    // Arrange and Act
    ResourceSelectorContainer actualResourceSelectorContainer = new ResourceSelectorContainer();

    // Assert
    Location location = actualResourceSelectorContainer.getLocation();
    assertNull(location.getFileName());
    assertNull(actualResourceSelectorContainer.getDescription());
    assertNull(actualResourceSelectorContainer.getProject());
    assertNull(actualResourceSelectorContainer.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link ResourceSelectorContainer#add(ResourceSelector)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link ResourceSelectorContainer#ResourceSelectorContainer()} ResourceSelectors Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#add(ResourceSelector)}
   */
  @Test
  public void testAdd_whenNull_thenResourceSelectorContainerResourceSelectorsEmpty() {
    // Arrange
    ResourceSelectorContainer resourceSelectorContainer = new ResourceSelectorContainer();

    // Act
    resourceSelectorContainer.add(null);

    // Assert that nothing has changed
    assertFalse(resourceSelectorContainer.getSelectors().hasNext());
    assertTrue(resourceSelectorContainer.getResourceSelectors().isEmpty());
  }

  /**
   * Test {@link ResourceSelectorContainer#hasSelectors()}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenAndWithRIsAny_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new And(Type.ANY)).hasSelectors());
  }

  /**
   * Test {@link ResourceSelectorContainer#hasSelectors()}.
   * <ul>
   *   <li>Given {@link ResourceSelectorContainer#ResourceSelectorContainer()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenResourceSelectorContainer_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ResourceSelectorContainer()).hasSelectors());
  }

  /**
   * Test {@link ResourceSelectorContainer#selectorCount()}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#selectorCount()}
   */
  @Test
  public void testSelectorCount_givenAndWithRIsAny_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new And(Type.ANY)).selectorCount());
  }

  /**
   * Test {@link ResourceSelectorContainer#selectorCount()}.
   * <ul>
   *   <li>Given {@link ResourceSelectorContainer#ResourceSelectorContainer()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#selectorCount()}
   */
  @Test
  public void testSelectorCount_givenResourceSelectorContainer_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ResourceSelectorContainer()).selectorCount());
  }

  /**
   * Test {@link ResourceSelectorContainer#getSelectors()}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>Then next return {@link Type}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#getSelectors()}
   */
  @Test
  public void testGetSelectors_givenAndWithRIsAny_thenNextReturnType() {
    // Arrange and Act
    Iterator<ResourceSelector> actualSelectors = (new And(Type.ANY)).getSelectors();

    // Assert
    ResourceSelector nextResult = actualSelectors.next();
    assertTrue(nextResult instanceof Type);
    assertFalse(actualSelectors.hasNext());
    assertTrue(nextResult.isSelected(null));
  }

  /**
   * Test {@link ResourceSelectorContainer#getSelectors()}.
   * <ul>
   *   <li>Given {@link ResourceSelectorContainer#ResourceSelectorContainer()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#getSelectors()}
   */
  @Test
  public void testGetSelectors_givenResourceSelectorContainer() {
    // Arrange, Act and Assert
    assertFalse((new ResourceSelectorContainer()).getSelectors().hasNext());
  }

  /**
   * Test {@link ResourceSelectorContainer#getResourceSelectors()}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#getResourceSelectors()}
   */
  @Test
  public void testGetResourceSelectors_givenAndWithRIsAny_thenReturnSizeIsOne() {
    // Arrange and Act
    List<ResourceSelector> actualResourceSelectors = (new And(Type.ANY)).getResourceSelectors();

    // Assert
    assertEquals(1, actualResourceSelectors.size());
    assertTrue(actualResourceSelectors.get(0) instanceof Type);
  }

  /**
   * Test {@link ResourceSelectorContainer#getResourceSelectors()}.
   * <ul>
   *   <li>Given {@link ResourceSelectorContainer#ResourceSelectorContainer()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceSelectorContainer#getResourceSelectors()}
   */
  @Test
  public void testGetResourceSelectors_givenResourceSelectorContainer_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new ResourceSelectorContainer()).getResourceSelectors().isEmpty());
  }
}

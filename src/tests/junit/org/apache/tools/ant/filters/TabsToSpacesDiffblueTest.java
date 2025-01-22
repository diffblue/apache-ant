package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;

public class TabsToSpacesDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TabsToSpaces#TabsToSpaces(Reader)}
   *   <li>{@link TabsToSpaces#setTablength(int)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    TabsToSpaces actualTabsToSpaces = new TabsToSpaces(new StringReader("foo"));
    actualTabsToSpaces.setTablength(3);

    // Assert
    assertNull(actualTabsToSpaces.getParameters());
    assertNull(actualTabsToSpaces.getProject());
    assertFalse(actualTabsToSpaces.getInitialized());
  }

  /**
   * Test {@link TabsToSpaces#TabsToSpaces()}.
   * <p>
   * Method under test: {@link TabsToSpaces#TabsToSpaces()}
   */
  @Test
  public void testNewTabsToSpaces() {
    // Arrange and Act
    TabsToSpaces actualTabsToSpaces = new TabsToSpaces();

    // Assert
    assertNull(actualTabsToSpaces.getParameters());
    assertNull(actualTabsToSpaces.getProject());
    assertFalse(actualTabsToSpaces.getInitialized());
  }

  /**
   * Test {@link TabsToSpaces#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link TabsToSpaces#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    TabsToSpaces tabsToSpaces = new TabsToSpaces(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, tabsToSpaces.read());
    assertTrue(tabsToSpaces.getInitialized());
  }

  /**
   * Test {@link TabsToSpaces#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link TabsToSpaces}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TabsToSpaces#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnTabsToSpaces() throws IOException {
    // Arrange
    TabsToSpaces tabsToSpaces = new TabsToSpaces();

    // Act
    Reader actualChainResult = tabsToSpaces.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof TabsToSpaces);
    assertEquals("foo", ((TabsToSpaces) actualChainResult).readFully());
    assertNull(((TabsToSpaces) actualChainResult).getParameters());
    assertNull(((TabsToSpaces) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((TabsToSpaces) actualChainResult).getInitialized());
  }
}
